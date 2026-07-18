(ns cherry.compiler.node
  (:require
   ["fs" :as fs]
   ["path" :as path]
   [cherry.compiler :as compiler]
   [clojure.string :as str]
   [edamame.core :as e]
   [shadow.esm :as esm]
   [squint.compiler-common :as cc]
   [squint.internal.node.utils :as utils]))

(def sci (atom nil))

(def config-file "cherry.edn")

(defn slurp [f]
  (fs/readFileSync f "utf-8"))

(defn spit [f s]
  (fs/writeFileSync f s "utf-8"))

(defn scan-macros [s {:keys [ns-state]}]
  (let [maybe-ns (e/parse-next (e/reader s) compiler/cherry-parse-opts)]
    (when (and (seq? maybe-ns)
               (= 'ns (first maybe-ns)))
      (let [[_ns the-ns-name & clauses] maybe-ns
            [require-macros reload] (some (fn [[clause reload]]
                                            (when (and (seq? clause)
                                                       (= :require-macros (first clause)))
                                              [(rest clause) reload]))
                                          (partition-all 2 1 clauses))]
        (when require-macros
          (.then (esm/dynamic-import "./compiler.sci.js")
                 (fn [_]
                   (let [eval-form (:eval-form @sci)]
                     (reduce
                      (fn [prev require-macros]
                        (.then prev
                               (fn [_]
                                 (let [[macro-ns & {:keys [refer as]}] require-macros
                                       actual-ns (cc/resolve-macro-ns macro-ns :cherry)
                                       file (utils/resolve-file actual-ns (:paths (utils/get-cfg config-file) ["." "src"]))
                                       built-in (get compiler/built-in-macro-nss actual-ns)
                                       macros (cond
                                                file
                                                (js/Promise.resolve
                                                 (do (eval-form (cond-> (list 'require (list 'quote actual-ns))
                                                                  reload (concat [:reload])))
                                                     (let [publics (eval-form
                                                                    `(ns-publics '~actual-ns))
                                                           macros (keep (fn [[k v]]
                                                                          (when (:macro (meta v))
                                                                            [k (deref v)])) publics)
                                                           macros (into {} macros)]
                                                       macros)))
                                                built-in
                                                (js/Promise.resolve built-in)
                                                :else
                                                (throw (js/Error. (str "Could not locate macro namespace " actual-ns))))]
                                   (.then macros
                                          (fn [macros]
                                            (swap! ns-state (fn [ns-state]
                                                              (cond-> (assoc-in ns-state [:macros actual-ns] macros)
                                                                as (-> (assoc-in [the-ns-name :aliases as] macro-ns)
                                                                       (assoc-in [the-ns-name :macro-aliases as] macro-ns))
                                                                refer (update-in [the-ns-name :refers]
                                                                                 merge
                                                                                 (zipmap refer (repeat macro-ns))))))))))))
                      (js/Promise.resolve nil)
                      require-macros)))))))))

(defn default-ns-state []
  (atom {:current 'user}))

(defn ->opts [opts]
  (assoc opts :ns-state (or (:ns-state opts) (default-ns-state))))

(defn compile-string [contents opts]
  (let [opts (->opts opts)]
    (-> (js/Promise.resolve (scan-macros contents opts))
        (.then #(compiler/compile-string* contents opts)))))

(def in-dir? utils/in-dir?)

(def adjust-file-for-paths utils/adjust-file-for-paths)

(defn file-in-output-dir [file paths output-dir]
  (if output-dir
    (path/resolve output-dir
                  (adjust-file-for-paths file paths))
    file))

(defn- with-output-extension
  ;; Swap to `extension` (default .mjs) and use "/" for ESM specifiers.
  [file extension]
  (let [ext (or extension ".mjs")
        ext (if (str/starts-with? ext ".") ext (str "." ext))]
    (-> file
        (str/replace (re-pattern (str (path/extname file) "$")) ext)
        (str/replace "\\" "/"))))

(defn- compiled-output-path
  ;; Absolute path to ns `x`'s compiled output module, or nil for a non-local ns.
  [x paths output-dir extension]
  (some-> (utils/resolve-file x paths)
          (file-in-output-dir paths output-dir)
          (with-output-extension extension)))

(defn resolve-ns-repl
  "Like the CLI's resolve-ns but returns an absolute path. The REPL evals in
  cherry's lib dir where a relative specifier cannot resolve."
  [x]
  (let [{:keys [output-dir paths extension]} (utils/expand-paths (or (utils/get-cfg config-file) {}))]
    (compiled-output-path x (or paths ["." "src"]) (or output-dir ".") extension)))

(defn compile-file [{:keys [in-file in-str out-file extension output-dir]
                     :or {output-dir ""}
                     :as opts}]
  (let [contents (or in-str (slurp in-file))
        opts (->opts opts)]
    (-> (compile-string contents (assoc opts :ns nil))
        (.then (fn [{:keys [javascript jsx] :as opts}]
                 (let [paths (:paths @utils/!cfg ["." "src"])
                       out-file (path/resolve output-dir
                                              (or out-file
                                                  (str/replace (adjust-file-for-paths in-file paths) #".clj(s|c)$"
                                                               (if jsx
                                                                 ".jsx"
                                                                 (or (when-let [ext extension]
                                                                       (str "." (str/replace ext #"^\." "")))
                                                                     ".mjs")))))
                       out-path (path/dirname out-file)]
                   (when-not (fs/existsSync out-path)
                     (fs/mkdirSync out-path #js {:recursive true}))
                   (when-not (fs/existsSync out-path)
                     (throw (js/Error. "File not found, make sure output-dir is a valid path: "
                                       {:output-dir output-dir
                                        :out-file out-file})))
                   (spit out-file javascript)
                   (assoc opts :out-file out-file)))))))

(defn ->clj [x]
  (js->clj x :keywordize-keys true))

(defn- jsify [f]
  (fn [& args]
    (let [args (mapv ->clj args)
          ret (apply f args)]
      (if (instance? js/Promise ret)
        (.then ret clj->js)
        (clj->js ret)))))

#_{:clj-kondo/ignore [:unused-private-var]}
(def ^:private compile-string-js
  (jsify compile-string))

#_{:clj-kondo/ignore [:unused-private-var]}
(def ^:private compile-file-js
  (jsify compile-file))

