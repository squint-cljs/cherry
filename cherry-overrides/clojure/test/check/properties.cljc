(ns clojure.test.check.properties)

(defn- binding-vars
  "Extract variable names from let-style bindings vector."
  [bindings]
  (map first (partition 2 bindings)))

(defn- binding-gens
  "Extract generator expressions from let-style bindings vector."
  [bindings]
  (map second (partition 2 bindings)))

(defmacro for-all
  [bindings & body]
  (let [for-all-sym 'for-all*]
    `(~for-all-sym ~(vec (binding-gens bindings))
                   (fn [~@(binding-vars bindings)]
                     ~@body))))
