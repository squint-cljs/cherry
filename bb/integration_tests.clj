(ns integration-tests
  (:require [babashka.process :as p :refer [sh shell]]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is]]))

(deftest macro-test
  (let [out (:out (sh {:err :inherit
                       :dir "test-resources/test_project"}
                   "npx cherry run macro_test.cljs"))]
    (is (str/includes? out "22"))))

(deftest equality-test
  (let [out (:out (sh {:err :inherit
                       :dir "test-resources/test_project"}
                      "npx cherry run equality_test.cljs"))]
    (is (str/includes? out "[false true]"))))

(deftest repl-api-test
  (let [out (:out (p/shell {:out :string} "node test-resources/js_api.mjs"))]
    (is (= ["1" "1"]  (str/split-lines out)))))

(defn run-tests []
  (shell {:dir "test-resources/test_project"} "npm install")
  (let [{:keys [fail error]} (t/run-tests 'integration-tests)]
    (when (and fail error (pos? (+ fail error)))
      (throw (ex-info "Tests failed" {:babashka/exit 1})))))
