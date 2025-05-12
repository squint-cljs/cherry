(ns cherry.embed-test
  (:require [cherry.embed :as e]
            [clojure.test :as t :refer [deftest is]]))

(deftest embed-test
  (is (= 6 (e/eval-form '(+ 1 2 3))))
  (is (= 6 (e/eval-string (str '(+ 1 2 3))))))

