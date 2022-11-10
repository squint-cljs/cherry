(ns integration-tests
  (:require [babashka.process :refer [sh shell]]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is]]))

(deftest macro-test
  (let [out (:out (sh "npx cherry run macro_test.cljs" {:err :inherit
                                                        :dir "test-resources/test_project"}))]
    (is (str/includes? out "22"))))

(deftest equality-test
  (let [out (:out (sh "npx cherry run equality_test.cljs" {:err :inherit
                                                           :dir "test-resources/test_project"}))]
    (is (str/includes? out "[false true]"))))

(defn run-tests []
  (shell {:dir "test-resources/test_project"} "npm install")
  (let [{:keys [fail error]} (t/run-tests 'integration-tests)]
    (when (and fail error (pos? (+ fail error)))
      (throw (ex-info "Tests failed" {:babashka/exit 1})))))
