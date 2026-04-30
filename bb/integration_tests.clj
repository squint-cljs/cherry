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

(deftest cross-platform-cherry-test
  (let [out (:out (sh {:err :inherit}
                      "node lib/cli.js run test/cherry/cross_platform_test.cljc"))]
    (is (str/includes? out "0 failures") "cross-platform test passes on Cherry")))

(deftest cljc-macro-reads-data-ns-via-require-macros-test
  (let [{:keys [out exit]} (sh {:dir "test-resources/test_project"
                                :out :string :err :inherit}
                               "npx cherry run macro_data_via_require_macros_test.cljs")]
    (is (zero? exit) "compile + run completes without sci error")
    (is (str/includes? out ":fn-key :add") "macro expanded — add called")
    (is (str/includes? out ":fn-key :mul") "macro expanded — mul called")))

(deftest cljc-macro-via-require-expands-at-compile-time-test
  (let [dir "test-resources/test_project"
        out-mjs (str dir "/macro_data_via_require_test.mjs")]
    (shell "rm" "-f" out-mjs)
    (sh {:dir dir :err :inherit}
        "npx cherry compile src/macro_data.cljc src/macro_data_macros.cljc macro_data_via_require_test.cljc")
    (let [emitted (slurp out-mjs)]
      (is (str/includes? emitted "var add = function") "macro expanded inline — add defn emitted")
      (is (str/includes? emitted "var mul = function") "macro expanded inline — mul defn emitted"))))

(deftest cljc-macro-via-require-runs-test
  (let [dir "test-resources/test_project"]
    (sh {:dir dir :err :inherit}
        "npx cherry compile src/macro_data.cljc src/macro_data_macros.cljc macro_data_via_require_test.cljc")
    (let [{:keys [out exit]} (sh {:dir dir :out :string :err :inherit}
                                 "node macro_data_via_require_test.mjs")]
      (is (zero? exit) "consumer runs without ERR_MODULE_NOT_FOUND")
      (is (str/includes? out ":fn-key :add") "add prints data-driven body")
      (is (str/includes? out ":fn-key :mul") "mul prints data-driven body"))))

(defn run-tests []
  (shell {:dir "test-resources/test_project"} "npm install")
  (let [{:keys [fail error]} (t/run-tests 'integration-tests)]
    (when (and fail error (pos? (+ fail error)))
      (throw (ex-info "Tests failed" {:babashka/exit 1})))))
