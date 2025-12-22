(ns cherry.cross-platform-test
  (:require [clojure.test :as t #?@(:clj [:refer [deftest is testing are]])]
            [clojure.string :as str])
  #?(:cljs (:require-macros [clojure.test :as t :refer [deftest is testing are]])))

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

(deftest are-test
  (testing "table-driven tests"
    (are [x y expected] (= expected (+ x y))
      1 1 2
      2 3 5
      10 20 30)))

#?(:cljs
   (deftest are-runs-all-cases-test
     (testing "are runs every case"
       (let [saved-env (t/get-current-env)
             saved-counters (:report-counters saved-env)]
         (t/set-env! (assoc (t/empty-env) :testing-vars (:testing-vars saved-env)))
         (let [pass-before (get-in (t/get-current-env) [:report-counters :pass] 0)]
           (are [x] (pos? x)
             1 2 3 4 5)
           (let [pass-after (get-in (t/get-current-env) [:report-counters :pass] 0)
                 pass-count (- pass-after pass-before)]
             (t/set-env! (assoc saved-env :report-counters saved-counters))
             (when-not (= 5 pass-count)
               (throw (js/Error. (str "are should run all 5 cases, but ran " pass-count))))
             (is true "are correctly runs all cases")))))))

(deftest exception-test
  (testing "thrown?"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (throw #?(:clj (Exception. "test") :cljs (js/Error. "test"))))))
  (testing "thrown-with-msg?"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                          #"test"
                          (throw #?(:clj (Exception. "test") :cljs (js/Error. "test")))))))

#?(:cljs
   (deftest verify-thrown-reports-missing-exception
     (testing "thrown? fails when no exception thrown"
       (let [saved-env (t/get-current-env)
             saved-counters (:report-counters saved-env)]
         (t/set-env! (assoc (t/empty-env) :testing-vars (:testing-vars saved-env)))
         (let [fail-before (get-in (t/get-current-env) [:report-counters :fail] 0)]
           (is (thrown? js/Error (+ 1 1)))
           (let [fail-after (get-in (t/get-current-env) [:report-counters :fail] 0)]
             (t/set-env! (assoc saved-env :report-counters saved-counters))
             (when-not (> fail-after fail-before)
               (throw (js/Error. (str "thrown? should fail when no exception - fail-before: " fail-before
                                      " fail-after: " fail-after))))
             (is true "thrown? correctly fails when no exception")))))))

#?(:cljs
   (deftest verify-thrown-with-msg-checks-pattern
     (testing "thrown-with-msg? fails on wrong message pattern"
       (let [saved-env (t/get-current-env)
             saved-counters (:report-counters saved-env)]
         (t/set-env! (assoc (t/empty-env) :testing-vars (:testing-vars saved-env)))
         (let [fail-before (get-in (t/get-current-env) [:report-counters :fail] 0)]
           (is (thrown-with-msg? js/Error #"WRONG PATTERN"
                                 (throw (js/Error. "actual message"))))
           (let [fail-after (get-in (t/get-current-env) [:report-counters :fail] 0)]
             (t/set-env! (assoc saved-env :report-counters saved-counters))
             (when-not (> fail-after fail-before)
               (throw (js/Error. (str "thrown-with-msg? should fail on wrong pattern - fail-before: " fail-before
                                      " fail-after: " fail-after))))
             (is true "thrown-with-msg? correctly fails on wrong pattern")))))))

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
     (t/test-var are-test)
     (t/test-var are-runs-all-cases-test)
     (t/test-var exception-test)
     (t/test-var verify-thrown-reports-missing-exception)
     (t/test-var verify-thrown-with-msg-checks-pattern)
     (t/test-var verify-is-reports-failures)
     (t/test-var verify-assert-expr-default-reports-failures)
     (t/test-var testing-context-test)
     (t/report {:type :summary})
     (let [results (:report-counters (t/get-current-env))]
       (when-not (t/successful? results)
         (js/process.exit 1)))))

#?(:cljs (-main))
