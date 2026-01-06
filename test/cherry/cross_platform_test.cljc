(ns cherry.cross-platform-test
  (:require [clojure.test :as t #?@(:clj [:refer [deftest is testing are]])]
            [clojure.string :as str]
            #?@(:cljs [[clojure.test.check :refer [quick-check]]
                       [clojure.test.check.generators :as gen]
                       [clojure.test.check.properties :refer [for-all*]]]
                :clj [[clojure.test.check.generators :as gen]
                      [clojure.test.check.properties :as prop]]))
  #?(:cljs (:require-macros [clojure.test :as t :refer [deftest deftest- is testing are]]
                            [clojure.test.check.clojure-test :refer [defspec]]
                            [clojure.test.check.properties :refer [for-all]])))

(defonce test-db (atom nil))

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

(deftest join-fixtures-empty-test
  (testing "join-fixtures with empty list just calls test"
    (let [called (atom false)
          joined (t/join-fixtures [])]
      (joined (fn [] (reset! called true)))
      (is @called "test function should be called even with no fixtures"))))

(deftest fixtures-test
  (testing "compose-fixtures nests first around second"
    (let [users-fixture (fn [f]
                          (reset! test-db {:users {}})
                          (f)
                          (reset! test-db nil))
          data-fixture (fn [f]
                         (swap! test-db assoc-in [:users :user-1] {:name "Alice"})
                         (f))
          composed (t/compose-fixtures users-fixture data-fixture)]
      (composed (fn []
                  (is (= "Alice" (get-in @test-db [:users :user-1 :name])))))
      (is (nil? @test-db) "fixture teardown clears database")))
  (testing "join-fixtures does the same for a collection"
    (let [users-fixture (fn [f]
                          (reset! test-db {})
                          (f)
                          (reset! test-db nil))
          alice-fixture (fn [f]
                          (swap! test-db assoc :user-1 {:name "Alice" :age 30})
                          (f))
          bob-fixture (fn [f]
                        (swap! test-db assoc :user-2 {:name "Bob" :age 25})
                        (f))
          joined (t/join-fixtures [users-fixture alice-fixture bob-fixture])]
      (joined (fn []
                (is (= "Alice" (get-in @test-db [:user-1 :name])))
                (is (= "Bob" (get-in @test-db [:user-2 :name])))
                (is (= 2 (count @test-db)))))
      (is (nil? @test-db) "fixture teardown clears database"))))

#?(:cljs
   (deftest each-fixtures-applied-test
     (testing "each fixtures provide fresh database for each test"
       (let [fixture (fn [test-fn]
                       (reset! test-db {:user-1 {:name "Alice" :age 30}})
                       (test-fn)
                       (reset! test-db nil))
             test-1 (with-meta
                      (fn []
                        (is (= "Alice" (get-in @test-db [:user-1 :name])))
                        (swap! test-db assoc :user-1 {:name "Modified"}))
                      {:name 'test-1})
             test-2 (with-meta
                      (fn []
                        (is (= "Alice" (get-in @test-db [:user-1 :name])) "fixture gives fresh DB"))
                      {:name 'test-2})]
         (t/set-each-fixtures! [fixture])
         (t/test-var test-1)
         (t/test-var test-2)
         (t/set-each-fixtures! [])
         (is (nil? @test-db) "fixture teardown clears database")))))

#?(:cljs
   (deftest once-fixtures-with-run-tests-test
     (testing "once fixtures set up database once for all tests"
       (let [saved-env (t/get-current-env)
             setup-count (atom 0)
             teardown-count (atom 0)
             fixture (fn [test-fn]
                       (swap! setup-count inc)
                       (reset! test-db {:user-1 {:name "Alice" :age 30}
                                        :user-2 {:name "Bob" :age 25}})
                       (test-fn)
                       (reset! test-db nil)
                       (swap! teardown-count inc))
             test-1 (with-meta
                      (fn []
                        (is (= "Alice" (get-in @test-db [:user-1 :name])))
                        (is (= 30 (get-in @test-db [:user-1 :age]))))
                      {:name 'test-1})
             test-2 (with-meta
                      (fn []
                        (is (= "Bob" (get-in @test-db [:user-2 :name])))
                        (is (= 25 (get-in @test-db [:user-2 :age]))))
                      {:name 'test-2})]
         (t/set-env! (t/empty-env))
         (t/set-once-fixtures! [fixture])
         (t/run-tests test-1 test-2)
         (t/set-env! saved-env)
         (is (= 1 @setup-count) "setup runs exactly once")
         (is (= 1 @teardown-count) "teardown runs exactly once")))))

#?(:cljs
   (deftest ^:async async-test
     (testing "async with setTimeout"
       (js-await
        (js/Promise.
         (fn [resolve]
           (js/setTimeout
            (fn []
              (is (= 1 1))
              (resolve))
            10)))))))

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

#?(:cljs
   (deftest successful-test
     (testing "successful? is false with failures"
       (is (not (t/successful? {:fail 1 :error 0}))))
     (testing "successful? is false with errors"
       (is (not (t/successful? {:fail 0 :error 1}))))
     (testing "successful? is true when both zero"
       (is (t/successful? {:fail 0 :error 0})))))

#?(:cljs
   (deftest test-var-counter-test
     (testing "test-var increments :test counter"
       (let [test-before (get-in (t/get-current-env) [:report-counters :test] 0)]
         (t/test-var (fn [] nil))
         (let [test-after (get-in (t/get-current-env) [:report-counters :test] 0)]
           (is (= 1 (- test-after test-before))))))))

#?(:cljs
   (deftest ^:async wrap-async-fixture-test
     (testing "wrap-async waits for async test before teardown"
       (let [log (atom [])
             fixture (t/wrap-async
                      #(swap! log conj :setup)
                      #(swap! log conj :teardown))
             async-test (fn []
                          (js/Promise.
                           (fn [resolve]
                             (js/setTimeout
                              (fn []
                                (swap! log conj :test-done)
                                (resolve))
                              20))))
             result (fixture async-test)]
         (is (instance? js/Promise result))
         (js-await result)
         (js-await (js/Promise. (fn [resolve] (js/setTimeout resolve 30))))
         (is (= [:setup :test-done :teardown] @log)
             "teardown should wait for async test")))))

#?(:cljs
   (deftest- ^:async private-async-helper
     (js-await (js/Promise. (fn [resolve] (js/setTimeout resolve 5))))
     (is true)))

#?(:cljs
   (deftest deftest-private-async-test
     (testing "deftest- works with ^:async"
       (let [result (t/test-var private-async-helper)]
         (is (instance? js/Promise result) "deftest- ^:async returns Promise")))))

#?(:cljs
   (deftest ^:async run-tests-async-test
     (testing "run-tests chains async tests correctly"
       (let [log (atom [])
             saved-fixtures (t/get-once-fixtures)]
         (t/set-once-fixtures! [(t/wrap-async
                                 #(swap! log conj :setup)
                                 #(swap! log conj :teardown))])
         (let [sync-first (with-meta
                            (fn []
                              (swap! log conj :test-1)
                              (is (= 1 1) "sync-first assertion"))
                            {:name 'sync-first})
               async-delayed (with-meta
                               (fn []
                                 (js/Promise.
                                  (fn [resolve]
                                    (js/setTimeout
                                     (fn []
                                       (swap! log conj :test-2)
                                       (is (pos? 42) "async-delayed assertion")
                                       (resolve))
                                     20))))
                               {:name 'async-delayed})
               sync-last (with-meta
                           (fn []
                             (swap! log conj :test-3)
                             (is (string? "yes") "sync-last assertion"))
                           {:name 'sync-last})
               result (t/run-tests sync-first async-delayed sync-last)]
           (js-await result)
           (js-await (js/Promise. (fn [resolve] (js/setTimeout resolve 30))))
           (is (= [:setup :test-1 :test-2 :test-3 :teardown] @log)
               "run-tests should chain async tests in order")
           (t/set-once-fixtures! saved-fixtures))))))

#?(:clj
   (defn -main []
     (let [result (t/run-tests 'cherry.cross-platform-test)]
       (when-not (t/successful? result)
         (System/exit 1))))
   :cljs
   (defn ^:async -main []
     (t/set-env! (t/empty-env))
     (t/test-var arithmetic-test)
     (t/test-var equality-test)
     (t/test-var are-test)
     (t/test-var are-runs-all-cases-test)
     (t/test-var exception-test)
     (t/test-var verify-thrown-reports-missing-exception)
     (t/test-var verify-thrown-with-msg-checks-pattern)
     (t/test-var join-fixtures-empty-test)
     (t/test-var fixtures-test)
     (t/test-var each-fixtures-applied-test)
     (t/test-var once-fixtures-with-run-tests-test)
     (js-await (t/test-var async-test))
     (t/test-var verify-is-reports-failures)
     (t/test-var verify-assert-expr-default-reports-failures)
     (t/test-var testing-context-test)
     (t/test-var successful-test)
     (t/test-var test-var-counter-test)
     (js-await (t/test-var wrap-async-fixture-test))
     (t/test-var deftest-private-async-test)
     (js-await (t/test-var run-tests-async-test))
     (t/report {:type :summary})
     (let [results (:report-counters (t/get-current-env))]
       (when-not (t/successful? results)
         (js/process.exit 1)))))

#?(:cljs (-main))
