(ns cherry.cross-platform-test
  (:require [clojure.test :as t #?@(:clj [:refer [deftest is testing]])]
            [clojure.string :as str])
  #?(:cljs (:require-macros [clojure.test :as t :refer [deftest is testing]])))

(deftest arithmetic-test
  (testing "basic arithmetic with referred macros"
    (is (= 4 (+ 2 2)))
    (is (= 6 (* 2 3))))
  (t/testing "basic arithmetic with qualified macros"
    (t/is (= 10 (+ 5 5)))
    (t/is (= 12 (* 3 4)))))

(deftest equality-test
  (testing "value equality"
    (is (= [1 2 3] [1 2 3]))
    (is (= {:a 1} {:a 1}))))

#?(:cljs
   (deftest verify-is-reports-failures
     (testing "is reports failures"
       (let [saved-env (t/get-current-env)
             saved-counters (:report-counters saved-env)]
         (t/set-env! (assoc (t/empty-env) :testing-vars (:testing-vars saved-env)))
         (let [fail-count-before (get-in (t/get-current-env) [:report-counters :fail] 0)]
           (is (= 1 2))
           (let [fail-count-after (get-in (t/get-current-env) [:report-counters :fail] 0)
                 pass-count-after (get-in (t/get-current-env) [:report-counters :pass] 0)]
             (t/set-env! (assoc saved-env :report-counters saved-counters))
             (when-not (> fail-count-after fail-count-before)
               (throw (js/Error. (str "is should report failures - fail-count-before: " fail-count-before
                                      " fail-count-after: " fail-count-after
                                      " pass-count-after: " pass-count-after))))
             (is true "is correctly reports failures")))))))

#?(:cljs
   (deftest verify-assert-expr-default-reports-failures
     (testing "non-equality assertions report failures via assert-expr-default"
       (let [saved-env (t/get-current-env)
             saved-counters (:report-counters saved-env)]
         (t/set-env! (assoc (t/empty-env) :testing-vars (:testing-vars saved-env)))
         (let [fail-count-before (get-in (t/get-current-env) [:report-counters :fail] 0)]
           (is (neg? 1))
           (let [fail-count-after (get-in (t/get-current-env) [:report-counters :fail] 0)]
             (t/set-env! (assoc saved-env :report-counters saved-counters))
             (when-not (> fail-count-after fail-count-before)
               (throw (js/Error. (str "assert-expr-default should report failures - fail-count-before: " fail-count-before
                                      " fail-count-after: " fail-count-after))))
             (is true "assert-expr-default correctly reports failures")))))))

(deftest testing-context-test
  (testing "outer context"
    (is (str/includes? (t/testing-contexts-str) "outer context"))
    (testing "inner context"
      (is (str/includes? (t/testing-contexts-str) "inner context")))))

#?(:clj
   (defn -main []
     (let [result (t/run-tests 'cherry.cross-platform-test)]
       (when-not (t/successful? result)
         (System/exit 1))))
   :cljs
   (defn -main []
     (t/set-env! (t/empty-env))
     (t/test-var arithmetic-test)
     (t/test-var equality-test)
     (t/test-var verify-is-reports-failures)
     (t/test-var verify-assert-expr-default-reports-failures)
     (t/test-var testing-context-test)
     (t/report {:type :summary})
     (let [results (:report-counters (t/get-current-env))]
       (when-not (t/successful? results)
         (js/process.exit 1)))))

#?(:cljs (-main))
