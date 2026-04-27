(ns macro-data-via-require-macros-test
  (:require-macros [macro-data-macros :refer [define-fns]]))

(define-fns)

(prn (add 1))
(prn (mul 2))
