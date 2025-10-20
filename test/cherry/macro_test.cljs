(ns cherry.macro-test
  "Tests for runtime macro expansion via :macros option to compile-string.

  This tests the patch that enables macros to be provided as JavaScript
  functions and expanded at compile time."
  (:require
   [cherry.compiler :as cherry]
   [clojure.string :as str]
   [clojure.test :as t :refer [deftest is testing]]))

;; Define macro expansion functions
;; These are JavaScript functions that return the expanded AST
(defn my-macro-fn [_form _env x]
  #js ["str" "\"EXPANDED: \"" x])

(defn foo-macro-fn [_form _env data-ref & operations]
  #js ["cljs.core/hash-map"
       ":type" "\"foo-threading\""
       ":ref" data-ref
       ":ops" (.concat #js ["cljs.core/vector"] operations)])

(defn my-when-macro-fn [_form _env test & body]
  #js ["if" test (.concat #js ["do"] body)])

(deftest qualified-macro-test
  (testing "namespace-qualified macro call"
    (let [code "(my.ns/my-macro 42)"
          result (cherry/compile-string code #js {:macros #js {"my.ns" #js {"my-macro" my-macro-fn}}})]
      (is (str/includes? result "EXPANDED")
          "Qualified macro call should expand")
      (is (not (str/includes? result "my_macro"))
          "Should not contain function call"))))

(deftest unqualified-macro-test
  (testing "unqualified macro call (auto-refers)"
    (let [code "(my-macro 99)"
          result (cherry/compile-string code #js {:macros #js {"my.ns" #js {"my-macro" my-macro-fn}}})]
      (is (str/includes? result "EXPANDED")
          "Unqualified macro call should expand")
      (is (not (str/includes? result "my_macro"))
          "Should not contain function call"))))

(deftest macro-across-ns-change-test
  (testing "macro works after (ns ...) form"
    (let [code "(ns user)\n(my-macro 1)\n(ns other)\n(my-macro 2)"
          result (cherry/compile-string code #js {:macros #js {"my.ns" #js {"my-macro" my-macro-fn}}})]
      (is (str/includes? result "EXPANDED")
          "Macro should work after namespace change"))))

(deftest complex-macro-test
  (testing "macro with multiple arguments"
    (let [code "(foo-> :bar (blah 100) (baz 0.5))"
          result (cherry/compile-string code #js {:macros #js {"my.ns" #js {"foo->" foo-macro-fn}}})]
      (is (str/includes? result "foo-threading")
          "Complex macro should expand correctly")
      (is (str/includes? result ":ref")
          "Should contain expected keys"))))

(deftest variadic-macro-test
  (testing "macro that receives variable arguments"
    (let [code "(my-when (> x 10) (println \"yes\") (println \"ok\"))"
          result (cherry/compile-string code #js {:macros #js {"my.ns" #js {"my-when" my-when-macro-fn}}})]
      (is (str/includes? result "if")
          "my-when macro should expand to if")
      (is (not (str/includes? result "my_when"))
          "Should not contain my-when function call"))))
