(ns cherry.internal.cli
  (:require
   ["fs" :as fs]
   ["path" :as path]
   [babashka.cli :as cli]
   [cherry.compiler :as cc]
   [cherry.compiler.node :as compiler]
   [shadow.esm :as esm]
   [clojure.string :as str]))

(defn info [& xs]
  (apply *print-err-fn* xs))

(defn compile-files
  [opts files]
  (reduce (fn [prev f]
            (-> (js/Promise.resolve prev)
                (.then
                 #(do
                    (info "[cherry] Compiling CLJS file:" f)
                    (compiler/compile-file {:in-file f
                                            :repl (:repl opts)})))
                (.then (fn [{:keys [out-file]}]
                         (info "[cherry] Wrote JS file:" out-file)
                         out-file))))
          nil
          files))

(defn print-help []
  (println "Cherry v0.0.0

Usage:

run       <file.cljs>     Compile and run a file
compile   <file.cljs> ... Compile file(s)
help                      Print this help"))

(defn fallback [{:keys [rest-cmds opts]}]
  (if-let [e (:e opts)]
    (if (:help opts)
      (println "Usage: squint -e <expr> <opts>

Options:

--no-run: do not run compiled expression
--show:   print compiled expression")
      (let [e (if (:repl opts)
                (str/replace "(do %s\n)" "%s" e)
                e)
            res (cc/compile-string e (assoc opts :repl (:repl opts) :ns-state (atom {:current 'user})
                                            :context (if (:repl opts) :return :statement)
                                            :elide-exports (:repl opts)))
            res (if (:repl opts)
                  (str/replace "(async function() { %s })()" "%s" res)
                  res)
            dir (fs/mkdtempSync ".tmp")
            f (str dir "/squint.mjs")]
        (fs/writeFileSync f res "utf-8")
        (when (:show opts)
          (println res))
        (when-not (false? (:run opts))
          (let [path (if (path/isAbsolute f) f
                         (str (js/process.cwd) "/" f))]
            (-> (if (:repl opts)
                  (js/Promise.resolve (js/eval res))
                  (esm/dynamic-import path))
                (.then (fn [v]
                         (when (:repl opts)
                           (prn v))))
                (.finally (fn []
                            (fs/rmSync dir #js {:force true :recursive true}))))))))
    (if (or (:help opts)
            (= "help" (first rest-cmds))
            (empty? rest-cmds))
      (print-help)
      (compile-files opts rest-cmds))))

(defn run [{:keys [opts]}]
  (let [{:keys [file]} opts]
    (info "[cherry] Running" file)
    (.then (compiler/compile-file {:in-file file})
           (fn [{:keys [out-file]}]
             (esm/dynamic-import (str (js/process.cwd) "/" out-file))))))

#_(defn compile-form [{:keys [opts]}]
    (let [e (:e opts)]
      (println (t/compile! e))))

(def table
  [{:cmds ["run"]        :fn run :cmds-opts [:file]}
   {:cmds ["compile"]    :fn (fn [{:keys [opts rest-cmds]}]
                               (compile-files opts rest-cmds))}
   {:cmds []             :fn fallback}])

(defn init []
  (cli/dispatch table
                (.slice js/process.argv 2)
                {:aliases {:h :help}}))
