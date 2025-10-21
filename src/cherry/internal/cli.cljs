(ns cherry.internal.cli
  (:require
   ["fs" :as fs]
   ["path" :as path]
   ["url" :as url]
   [babashka.cli :as cli]
   [cherry.compiler :as cc]
   [cherry.compiler.node :as compiler]
   [clojure.string :as str]
   [shadow.esm :as esm]
   [squint.internal.node.utils :as utils]))

(defn info [& xs]
  (apply *print-err-fn* xs))

(defn file-in-output-dir [file paths output-dir]
  (if output-dir
    (path/resolve output-dir
                  (compiler/adjust-file-for-paths file paths))
    file))

(defn resolve-ns [opts in-file x]
  (let [output-dir (:output-dir opts)
        paths (:paths opts)
        in-file-in-output-dir (file-in-output-dir in-file paths output-dir)]
    (when-let [resolved
               (some-> (utils/resolve-file x)
                       (file-in-output-dir paths output-dir)
                       (some->> (path/relative (path/dirname (str in-file-in-output-dir)))))]
      (let [ext (:extension opts ".mjs")
            ext (if (str/starts-with? ext ".")
                  ext
                  (str "." ext))
            ext' (path/extname resolved)
            file (str "./" (str/replace resolved (re-pattern (str ext' "$")) ext))]
        file))))

(defn files-from-path [path]
  (let [files (fs/readdirSync path)]
    (vec (mapcat (fn [f]
                   (let [f (path/resolve path f)]
                     (if (.isDirectory (fs/lstatSync f))
                       (files-from-path f)
                       [f]))) files))))

(defn files-from-paths [paths]
  (vec (mapcat files-from-path paths)))

(defn match-file [x out-path]
  (cond (keyword? x)
        (re-find (re-pattern (str "\\." (name x) "$")) out-path)
        (string? x)
        (re-find (re-pattern x) out-path)))

(defn copy-file [copy-resources path output-dir paths]
  (when copy-resources
    (let [out-file (path/resolve output-dir (compiler/adjust-file-for-paths path paths))
          out-path (path/dirname out-file)]
      (when (some #(match-file % out-file)
                  copy-resources)
        (when-not (fs/existsSync out-path)
          (println "[cherry] Creating directory:" out-path)
          (fs/mkdirSync out-path #js {:recursive true}))
        (println "[cherry] Copying resource" path "to" out-path)
        (fs/copyFileSync path out-file)))))

(defn compile-files
  [opts files]
  (let [cfg @utils/!cfg
        opts (merge cfg opts)
        paths (:paths cfg)
        copy-resources (:copy-resources cfg)
        output-dir (:output-dir cfg ".")
        files (if (empty? files)
                (files-from-paths paths)
                files)]
    ;; shouldn't need this if :coerce worked in babashka.cli
    (when-let [out-dir (:output-dir opts)]
      (when-not (string? out-dir)
        (throw (js/Error. "output-dir must be a string"))))
    (if (:help opts)
      (do (println "Usage: squint compile <files> <opts>")
          (println)
          (println "Options:

--elide-imports: do not include imports
--elide-exports: do not include exports
--extension: default extension for JS files
--output-dir: output directory for JS files"))
      (reduce (fn [prev f]
                (-> (js/Promise.resolve prev)
                    (.then
                     #(do
                        (if (contains? #{".cljc" ".cljs"} (path/extname f ))
                          (do (println "[cherry] Compiling CLJS file:" f)
                              (compiler/compile-file (assoc opts
                                                            :in-file f
                                                            :resolve-ns (fn [x]
                                                                          (resolve-ns opts f x)))))
                          (copy-file copy-resources f output-dir paths))))
                    (.then (fn [{:keys [out-file]}]
                             (when out-file (println "[cherry] Wrote file:" out-file))
                             out-file))))
              nil
              files))))

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
      (let [e e #_(if (:repl opts)
                (str/replace "(do %s\n)" "%s" e)
                e)
            res (cc/compile-string e (assoc opts :repl (:repl opts) :ns-state (atom {:current 'user})
                                            :context (if (:repl opts) :return :statement)
                                            :elide-exports (and (:repl opts)
                                                                (not (false? (:elide-exports opts))))))
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
  (let [cfg @utils/!cfg
        opts (merge cfg opts)
        {:keys [file help]} opts]
    (if help
      nil
      (do (println "[cherry] Running" file)
          (-> (.then (compiler/compile-file (assoc opts :in-file file :resolve-ns (fn [x]
                                                                                    (resolve-ns opts file x))))
                     (fn [{:keys [out-file]}]
                       (let [path (if (path/isAbsolute out-file) out-file
                                      (path/resolve (js/process.cwd) out-file))
                             path (str (url/pathToFileURL path))]
                         (esm/dynamic-import path)))))))))

#_(defn compile-form [{:keys [opts]}]
    (let [e (:e opts)]
      (println (t/compile! e))))

(def table
  [{:cmds ["run"]        :fn run :args->opts [:file]}
   {:cmds ["compile"]    :fn (fn [{:keys [opts rest-cmds]}]
                               (compile-files opts rest-cmds))}
   {:cmds []             :fn fallback}])

(defn init []
  (cli/dispatch table
                (.slice js/process.argv 2)
                {:aliases {:h :help}
                 :coerce {:elide-exports :boolean
                          :elide-imports :boolean
                          :output-dir    :string
                          :repl          :boolean
                          :paths         [:string]
                          :extension     :string}}))
