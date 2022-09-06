(ns scratch (:require-macros [scratch-macros :refer [do-twice]]))

(do-twice (prn :foo))

