(ns macros)

(defn do-twice [_f _e x]
  `(try (do ~x ~x)
        (finally (prn :done))))
