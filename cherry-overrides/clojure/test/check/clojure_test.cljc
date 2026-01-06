(ns clojure.test.check.clojure-test)

(defmacro defspec [name num-tests-or-prop & rest]
  (let [[num-tests prop] (if (number? num-tests-or-prop)
                           [num-tests-or-prop (first rest)]
                           [100 num-tests-or-prop])]
    `(def ~(vary-meta name assoc :test true)
       (with-meta
         (fn []
           (let [result# (~'quick-check ~num-tests ~prop)]
             (if (:pass? result#)
               (~'report {:type :pass
                          :message (str "Passed " ~num-tests " trials")})
               (~'report {:type :fail
                          :message (str "Failed after " (:num-tests result#) " trials")
                          :expected '~prop
                          :actual (:shrunk result#)}))))
         {:name '~name}))))
