(ns macros-inner)

(defmacro inner [x]
  (list '* x x))
