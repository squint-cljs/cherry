;; Generates externs/cherry.txt: the protocol marker and method slot property
;; names cherry-emitted user code addresses literally, so Closure must not
;; rename them in the precompiled lib. Run via `bb gen-externs` after bumping
;; ClojureScript.

(require '[cljs.analyzer.api :as ana-api]
         '[clojure.string :as str])

(def header
  ["cljs$lang$protocol_mask$partition0$"
   "cljs$lang$protocol_mask$partition1$"
   "cljs$lang$ctorPrWriter"
   "cljs$lang$ctorStr"
   "cljs$lang$type"])

;; shadow's simple externs match by prefix, so the IFn marker would preserve
;; every cljs$core$IFn$_invoke$arity$N in the bundle: +120KB for the hottest
;; property in core. deftype IFn impls don't dispatch from the bundle either
;; way (apply and calls go through JS fallbacks), so IFn stays out.
(def excluded '#{IFn})

(def state (ana-api/empty-state))
(ana-api/analyze-file state "cljs/core.cljs" {})

(let [defs (:defs (ana-api/find-ns state 'cljs.core))
      protocols (sort-by first
                         (for [[sym info] defs
                               :when (and (:protocol-symbol info)
                                          (not (contains? excluded sym)))]
                           [sym info]))
      lines (for [[sym info] protocols
                  :let [prefix (str "cljs$core$" (munge (name sym)) "$")]
                  line (cons prefix
                             (sort
                              (for [[method sigs] (get-in info [:protocol-info :methods])
                                    sig sigs]
                                (str prefix (munge (name method)) "$arity$" (count sig)))))]
              line)]
  (spit "externs/cherry.txt"
        (str (str/join "\n" (concat header lines)) "\n"))
  (println "Wrote externs/cherry.txt:"
           (count protocols) "protocols,"
           (+ (count header) (count lines)) "lines")
  ;; cherry.internal.protocols/core-protocols drives the cljs$core$ prefix in
  ;; emitted user code and must cover the same protocols
  (let [src (slurp "src/cherry/internal/protocols.cljc")
        in-src (set (map symbol (str/split (second (re-find #"core-protocols '\#\{([^}]+)\}" src)) #"\s+")))
        missing (remove in-src (map first protocols))]
    (when (seq missing)
      (binding [*out* *err*]
        (println "Missing from cherry.internal.protocols/core-protocols:" (vec missing)))
      (System/exit 1))))
