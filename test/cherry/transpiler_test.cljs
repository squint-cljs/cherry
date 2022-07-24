(ns cherry.transpiler-test
  (:require
   [cherry.transpiler :as cherry]
   [clojure.test :as t :refer [deftest is]]))

(defn eval! [expr]
  (js/eval (doto (cherry/transpile-form expr) prn)))

(deftest let-test
  (is (= 3 (eval! '(let [x (do 1 2 3)] x)))))
