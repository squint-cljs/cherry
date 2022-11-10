(ns integration-tests
  (:require [babashka.process :refer [sh shell]]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is]]))

(deftest macro-test
  (shell {:dir "test-resources/macro_project"} "npm install")
  (let [out (:out (sh "npx cherry run main.cljs" {:dir "test-resources/macro_project"}))]
    (is (str/includes? out "22"))))

(defn run-tests []
  (let [{:keys [fail error]} (t/run-tests 'integration-tests)]
    (when (and fail error (pos? (+ fail error)))
      (throw (ex-info "Tests failed" {:babashka/exit 1})))))
