(ns integration-tests
  (:require [babashka.process :as p :refer [sh shell]]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is]]))

(deftest macro-test
  (let [out (:out (sh {:err :inherit
                       :dir "test-resources/test_project"}
                      "npx cherry run macro_test.cljs"))]
    (is (str/includes? out "22"))
    (is (str/includes? out "1"))))

(deftest equality-test
  (let [out (:out (sh {:err :inherit
                       :dir "test-resources/test_project"}
                      "npx cherry run equality_test.cljs"))]
    (is (str/includes? out "[false true]"))))

(deftest repl-api-test
  (let [out (:out (p/shell {:out :string} "node test-resources/js_api.mjs"))]
    (is (= ["1" "1"]  (str/split-lines out)))))

(deftest compile-test
  (let [tmp-file "/tmp/cherry-compile-test.cljs"
        out-file "/tmp/cherry-compile-test.mjs"]
    (spit tmp-file "(ns compile-test)\n(defn hello [] (js/console.log \"hello\"))")
    (shell "rm" "-f" out-file)
    (sh "npx" "cherry" "compile" tmp-file)
    (is (.exists (java.io.File. out-file)) "compile should create output file")))

(deftest cross-platform-jvm-test
  (let [{:keys [exit]} (sh {:err :inherit}
                           "clojure -M:test -n cherry.cross-platform-test")]
    (is (zero? exit) "cross-platform test passes on JVM")))

(deftest test-check-jvm-test
  (let [{:keys [exit]} (sh {:err :inherit}
                           "clojure -M:test -n cherry.test-check-test")]
    (is (zero? exit) "test.check tests pass on JVM")))

(deftest cross-platform-cherry-test
  (let [out (:out (sh {:err :inherit}
                      "node lib/cli.js run test/cherry/cross_platform_test.cljc"))]
    (is (str/includes? out "0 failures") "cross-platform test passes on Cherry")))

(deftest cross-platform-test-check-test
  (let [out (:out (sh {:err :inherit}
                      "node lib/cli.js run test/cherry/test_check_test.cljc"))]
    (is (str/includes? out "0 failures") "test.check tests pass on Cherry")))

(defn run-tests []
  (shell {:dir "test-resources/test_project"} "npm install")
  (let [{:keys [fail error]} (t/run-tests 'integration-tests)]
    (when (and fail error (pos? (+ fail error)))
      (throw (ex-info "Tests failed" {:babashka/exit 1})))))
