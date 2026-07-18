(ns cherry.compiler.node
  (:require
   ["crypto" :as crypto]
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

;; Tracks macro source files so we only re-eval (`:reload`) a macro namespace
;; when its file actually changed. Without this, the persistent SCI instance
;; keeps the first-loaded macro defs forever in watch mode (squint#819);
;; reloading on every compile would re-eval untouched macro nses instead of a
;; microsecond stat. path -> {:mtime _ :sha _}.
(def macro-state (atom {}))

(defn- sha256 [s]
  (-> (crypto/createHash "sha256") (.update s) (.digest "hex")))

(defn- macro-file [macro-ns]
  (utils/resolve-file macro-ns (:paths (utils/get-cfg config-file) ["." "src"])))

(defn- macro-file-changed?
  "True when macro-ns's source file changed since last seen. Cheap mtime gate
  first; only on mtime change do we read + sha256 to confirm real content
  change (suppresses spurious reloads on touch-without-edit). Updates cache."
  [macro-ns]
  (when-let [path (macro-file macro-ns)]
    (let [{:keys [mtime sha]} (get @macro-state path)
          cur-mtime (.-mtimeMs (fs/statSync path))]
      (when (not= cur-mtime mtime)
        (let [cur-sha (sha256 (fs/readFileSync path "utf-8"))]
          (swap! macro-state assoc path {:mtime cur-mtime :sha cur-sha})
          (not= cur-sha sha))))))

(defn slurp [f]
  (fs/readFileSync f "utf-8"))

(defn spit [f s]
  (fs/writeFileSync f s "utf-8"))

(defn- cljc-with-macros?
  "Check if a require clause refers to a .cljc file that contains defmacro."
  [libspec]
  (let [libname (if (symbol? libspec) libspec (first libspec))]
    (when (symbol? libname)
      (when-let [path (macro-file libname)]
        (and (str/ends-with? path ".cljc")
             (str/includes? (fs/readFileSync path "utf-8") "defmacro"))))))

(defn- as-alias? [libspec]
  (and (sequential? libspec)
       (some #{:as-alias} libspec)))

(defn- self-ref? [the-ns-name libspec]
  (and (sequential? libspec) (= the-ns-name (first libspec))))

(defn scan-macros [s {:keys [ns-state]}]
  (let [maybe-ns (e/parse-next (e/reader s) compiler/cherry-parse-opts)]
    (when (and (seq? maybe-ns)
               (= 'ns (first maybe-ns)))
      (let [[_ns the-ns-name & clauses] maybe-ns
            [require-macros reload] (some (fn [[clause reload]]
                                            (when (and (seq? clause)
                                                       (= :require-macros (first clause)))
                                              [(rest clause) reload]))
                                          (partition-all 2 1 clauses))
            ;; a regular :require of a .cljc that contains defmacro loads its
            ;; macros too, like squint
            require-libs (some->> clauses
                                  (some (fn [clause]
                                          (when (and (seq? clause)
                                                     (= :require (first clause)))
                                            (rest clause)))))
            require-cljc (->> require-libs
                              (remove #(self-ref? the-ns-name %))
                              (remove as-alias?)
                              (filter cljc-with-macros?))
            require-macros (concat require-macros require-cljc)]
        (when (seq require-macros)
          (.then (esm/dynamic-import "./compiler.sci.js")
                 (fn [_]
                   (let [eval-form (:eval-form @sci)]
                     (reduce
                      (fn [prev require-macros]
                        (.then prev
                               (fn [_]
                                 (let [;; a libspec may be a bare symbol, normalize to vector
                                       require-macros (if (symbol? require-macros)
                                                        [require-macros]
                                                        require-macros)
                                       [macro-ns & {:keys [refer as]}] require-macros
                                       actual-ns (cc/resolve-macro-ns macro-ns :cherry)
                                       file (macro-file actual-ns)
                                       built-in (get compiler/built-in-macro-nss actual-ns)
                                       reload? (or reload (macro-file-changed? actual-ns))
                                       macros (cond
                                                file
                                                (js/Promise.resolve
                                                 (do (eval-form (cond-> (list 'require (list 'quote actual-ns))
                                                                  reload? (concat [:reload])))
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

