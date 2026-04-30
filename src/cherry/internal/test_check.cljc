(ns cherry.internal.test-check)

(defn core-defspec [_&form _&env name num-tests-or-prop & rest]
  (let [[num-tests prop] (if (number? num-tests-or-prop)
                           [num-tests-or-prop (first rest)]
                           [100 num-tests-or-prop])]
    `(def ~(vary-meta name assoc :test true)
       (with-meta
         (fn []
           (let [result# (clojure.test.check/quick-check ~num-tests ~prop)]
             (if (:pass? result#)
               (clojure.test/report {:type :pass
                                     :message (str "Passed " ~num-tests " trials")})
               (clojure.test/report {:type :fail
                                     :message (str "Failed after " (:num-tests result#) " trials")
                                     :expected '~prop
                                     :actual (:shrunk result#)}))))
         {:name '~name}))))

(defn- binding-vars [bindings]
  (map first (partition 2 bindings)))

(defn- binding-gens [bindings]
  (map second (partition 2 bindings)))

(defn core-for-all [_&form _&env bindings & body]
  `(clojure.test.check.properties/for-all*
    ~(vec (binding-gens bindings))
    (fn [~@(binding-vars bindings)]
      ~@body)))

(def clojure-test-macros
  {'defspec core-defspec})

(def properties-macros
  {'for-all core-for-all})
