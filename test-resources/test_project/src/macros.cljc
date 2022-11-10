(ns macros)

(defmacro do! [& xs]
  (last xs))
