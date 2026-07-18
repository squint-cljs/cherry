(ns cherry.compiler.node
  (:require
   ["fs" :as fs]
   ["path" :as path]
   [cherry.compiler :as compiler]
   [clojure.string :as str]
   [shadow.esm :as esm]
   [squint.internal.node.macro-scan :as ms]
   [squint.internal.node.utils :as utils]))

(def sci (atom nil))

(def config-file "cherry.edn")


(defn slurp [f]
  (fs/readFileSync f "utf-8"))

(defn spit [f s]
  (fs/writeFileSync f s "utf-8"))

(def dialect
  {:parse-opts compiler/cherry-parse-opts
   :features #{:cherry :cljs :default}
   :config-file config-file
   :target :cherry
   :import-sci (fn [] (esm/dynamic-import "./compiler.sci.js"))
   :sci sci})

(def compile-time-source (partial ms/compile-time-source dialect))

(defn scan-macros [s opts]
  (ms/scan-macros dialect s opts))

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

(def file-in-output-dir utils/file-in-output-dir)

(def resolve-ns utils/resolve-ns)

(defn resolve-ns-repl [x]
  (utils/resolve-ns-repl x config-file))

(defn compile-file [{:keys [in-file in-str out-file extension output-dir]
                     :or {output-dir ""}
                     :as opts}]
  (let [contents (or in-str (slurp in-file))
        ;; merge the config file, resolve :deps into :paths and publish the
        ;; effective config, so macro-ns resolution sees dep paths too
        opts (utils/process-opts! config-file opts)
        opts (->opts opts)
        ;; When the caller didn't supply :resolve-ns, wire cherry's own
        ;; resolution so local ns requires import their compiled output.
        opts (cond-> opts
               (and in-file (not (:resolve-ns opts)))
               (assoc :resolve-ns (fn [x] (resolve-ns opts in-file x))))]
    (-> (compile-string contents (assoc opts :ns nil))
        (.then (fn [{:keys [javascript jsx] :as opts}]
                 (let [paths (:paths opts ["." "src"])
                       ;; Raw JSX tags need a .jsx extension so a downstream
                       ;; transform picks them up. With :jsx-runtime the output
                       ;; is plain JS (jsx() calls), so honor the configured
                       ;; extension instead.
                       jsx-tags? (and jsx (not (:jsx-runtime opts)))
                       out-file (path/resolve output-dir
                                              (or out-file
                                                  (str/replace (adjust-file-for-paths in-file paths) #".clj(s|c)$"
                                                               (if jsx-tags?
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
                   (cond-> (assoc opts :out-file out-file)
                     (:repl opts) (assoc :dev-hooks (utils/dev-hooks (:ns-state opts))))))))))

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

#_{:clj-kondo/ignore [:unused-private-var]}
(def ^:private read-config-js
  (jsify #(utils/read-config % config-file)))

#_{:clj-kondo/ignore [:unused-private-var]}
(def ^:private deps-paths-js
  (jsify #(utils/deps-paths % config-file)))

