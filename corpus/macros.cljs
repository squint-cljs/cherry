(ns macros)

(defn do-twice [_f _e x]
  `(do ~x ~x))
