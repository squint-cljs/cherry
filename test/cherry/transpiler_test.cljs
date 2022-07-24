(ns cherry.transpiler-test
  (:require
   [cherry.transpiler :as cherry]
   [clojure.string :as str]
   [clojure.test :as t :refer [deftest is]]))

(defn js! [expr]
  (let [js (cherry/transpile-form expr)]
    [(js/eval js) js]))

(deftest do-test
  (let [[v s] (js! '(do 1 2 3))]
    (is (= 3 v))
    (is (not (str/includes? s "function")))))

(deftest let-test
  (is (= 3 (first (js! '(let [x (do 1 2 3)] x))))))
