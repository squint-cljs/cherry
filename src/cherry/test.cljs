(ns cherry.test
  (:require [clojure.string]))

(def ^:dynamic *current-env* nil)

(defn empty-env []
  {:report-counters {:test 0 :pass 0 :fail 0 :error 0}
   :testing-vars ()
   :testing-contexts ()})

(defn get-current-env []
  (or *current-env* (empty-env)))

(defn set-env! [env]
  (set! *current-env* env)
  env)

(defn clear-env! []
  (set! *current-env* nil)
  nil)

(defn update-current-env! [ks f & args]
  (let [env (get-current-env)
        new-env (apply update-in env ks f args)]
    (set-env! new-env)))

(defn testing-contexts-str []
  (when-let [contexts (seq (:testing-contexts (get-current-env)))]
    (clojure.string/join " " (reverse contexts))))

(defn testing-vars-str []
  (when-let [vars (seq (:testing-vars (get-current-env)))]
    (clojure.string/join " " (map str vars))))

(defn inc-report-counter! [name]
  (when (:report-counters (get-current-env))
    (update-current-env! [:report-counters name] (fnil inc 0))))

(defn current-test-str []
  (let [vars (testing-vars-str)
        ctx (testing-contexts-str)]
    (cond
      (and vars ctx) (str vars " " ctx)
      vars vars
      ctx ctx
      :else "test")))

(defn report [{:keys [type message expected actual line column file] :as m}]
  (inc-report-counter! type)
  (let [location (when (or line column file)
                   (str (when file (str file ":"))
                        (when line line)
                        (when column (str ":" column))))]
    (case type
      :pass nil
      :fail (do
              (js/console.error (str "FAIL in " (current-test-str)
                                     (when location (str " (" location ")"))))
              (when message (js/console.error "  " message))
              (js/console.error "  expected:" (pr-str expected))
              (js/console.error "    actual:" (pr-str actual)))
      :error (do
               (js/console.error (str "ERROR in " (current-test-str)
                                      (when location (str " (" location ")"))))
               (when message (js/console.error "  " message))
               (when expected (js/console.error "  expected:" (pr-str expected)))
               (js/console.error "    actual:" (pr-str actual)))
      :begin-test-ns (js/console.log "\nTesting" (str (:ns m)))
      :end-test-ns nil
      :begin-test-var nil
      :end-test-var nil
      :summary (let [{:keys [test pass fail error]} (:report-counters (get-current-env))]
                 (js/console.log "\nRan" test "tests containing" (+ pass fail error) "assertions.")
                 (js/console.log (str fail) "failures," (str error) "errors."))
      (js/console.log "Unknown report type:" type m))))

(defn successful? [results]
  (and (zero? (:fail results 0))
       (zero? (:error results 0))))

(defn test-var [v]
  (when (fn? v)
    (let [test-name (or (:name (meta v)) "anonymous")
          pop-test-name! #(update-current-env! [:testing-vars] rest)]
      (update-current-env! [:testing-vars] conj test-name)
      (inc-report-counter! :test)
      (try
        (let [result (v)]
          (pop-test-name!)
          result)
        (catch :default e
          (pop-test-name!)
          (report {:type :error :message (.-message e) :expected nil :actual e}))))))
