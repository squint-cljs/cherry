(ns my-test
  (:require [clojure.test :as t])
  (:require-macros [clojure.test :refer [deftest is testing are]]))

(deftest are-test
  (testing "table-driven"
    (are [x y sum] (= sum (+ x y))
      1 1 2
      2 3 5
      10 20 30)))

(deftest thrown-test
  (testing "thrown?"
    (is (thrown? js/Error (throw (js/Error. "boom"))))
    (is (thrown-with-msg? js/Error #"boom"
          (throw (js/Error. "boom!"))))))

(deftest ^:async async-test
  (testing "async"
    (js/Promise.
     (fn [resolve]
       (js/setTimeout
        (fn []
          (is (= 42 (* 6 7)))
          (resolve))
        10)))))

(defn ^:async -main []
  (t/set-env! (t/empty-env))
  (t/test-var are-test)
  (t/test-var thrown-test)
  (js-await (t/test-var async-test))
  (t/report {:type :summary}))

(-main)
