#?(:clj  (ns macro-data-macros (:require [macro-data :as d]))
   :cljs (ns macro-data-macros (:require [macro-data :as d])))

(defmacro define-fns []
  `(do
     ~@(for [[fn-key fn-def] d/function-table]
         `(defn ~(symbol (name fn-key)) [~'x]
            {:fn-key ~fn-key
             :rettype ~(:rettype fn-def)
             :input ~'x}))))
