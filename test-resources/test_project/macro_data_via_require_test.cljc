#?(:clj  (ns macro-data-via-require-test
           (:require [macro-data-macros :refer [define-fns]]))
   :cljs (ns macro-data-via-require-test
           (:require [macro-data-macros :refer [define-fns]])))

(define-fns)

(prn (add 1))
(prn (mul 2))
