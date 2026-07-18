(ns nested-macro-test
  (:require-macros [macros-outer :refer [outer]]))

(prn (outer 5))
