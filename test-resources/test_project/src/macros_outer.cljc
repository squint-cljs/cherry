(ns macros-outer
  (:require [macros-inner :as inner]))

(defmacro outer [x]
  `(inner/inner ~x))
