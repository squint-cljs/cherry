(ns cherry.squint-and-cherry-test
  (:require
   [cherry.compiler :as cherry]
   [squint.compiler :as squint]
   [cherry.jsx-test]
   [cherry.test-utils :refer [js! jss! jsv!]]
   [clojure.string :as str]
   [clojure.test :as t :refer [async deftest is]]))

(deftest foo
  (prn :foo))

