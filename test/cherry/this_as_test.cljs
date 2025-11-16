(ns cherry.this-as-test
  (:require [clojure.test :refer [deftest is testing]]
            [cherry.test-utils :refer [jsv!]]
            [cherry.compiler :as cherry]))

(deftest deftype-object-method-test
  (testing "deftype with Object method that references this"
    (is (= 42 (jsv! '(do (deftype SimpleType [value]
                           Object
                           (getValue [this]
                             (.-value this)))
                         (def obj (SimpleType. 42))
                         (.getValue obj))))
        "Object method should access this correctly")))

(deftest deftype-protocol-method-test
  (testing "deftype with custom protocol method that references this"
    (is (= 123 (jsv! '(do (defprotocol IGetValue
                            (get-value [this]))
                          (deftype ProtoType [value]
                            IGetValue
                            (get-value [this]
                              (.-value this)))
                          (def obj (ProtoType. 123))
                          (get-value obj))))
        "Protocol method should access this correctly")))

(deftest deftype-multiple-this-refs-test
  (testing "deftype method with multiple references to this"
    (is (= 30 (jsv! '(do (deftype Calculator [x y]
                           Object
                           (sum [this]
                             (+ (.-x this) (.-y this))))
                         (def calc (Calculator. 10 20))
                         (.sum calc))))
        "Method should handle multiple this references")))

(deftest deftype-nested-this-test
  (testing "deftype method with nested function accessing this"
    (is (= 1 (jsv! '(do (deftype Container [items]
                          Object
                          (getFirst [this]
                            (let [xs (.-items this)]
                              (first xs))))
                        (def c (Container. [1 2 3]))
                        (.getFirst c))))
        "Nested access to this should work")))

(deftest compiled-output-no-invalid-statements-test
  (testing "compiled output should not contain invalid cljs.core.this_as statements"
    (let [compiled (cherry/compile-string "(deftype TestType [value]
                                             Object
                                             (getValue [this]
                                               (.-value this)))")]
      (is (not (re-find #"cljs\.core\.this_as" compiled))
          "Should not contain cljs.core.this_as reference")
      (is (not (re-find #"this_as;" compiled))
          "Should not contain standalone this_as; statement")
      (is (not (re-find #"this\$;" compiled))
          "Should not contain standalone this$; statement"))))

(deftest compiled-output-has-proper-bindings-test
  (testing "compiled output should have proper let bindings for this"
    (let [compiled (cherry/compile-string "(deftype TestType [value]
                                             Object
                                             (getValue [this]
                                               (.-value this)))")]
      (is (re-find #"const self__\s*=\s*this" compiled)
          "Should have self__ binding")
      (is (re-find #"const this\$" compiled)
          "Should have this$ binding"))))

(deftest extend-type-object-method-test
  (testing "extend-type with Object method using this"
    (is (= "MyType: 99" (jsv! '(do (deftype MyType [x])
                                   (extend-type MyType
                                     Object
                                     (toString [this]
                                       (str "MyType: " (.-x this))))
                                   (def obj (MyType. 99))
                                   (.toString obj))))
        "extend-type Object method should access this correctly")))

(deftest deftype-protocol-multi-method-test
  (testing "deftype with protocol having multiple methods using this"
    (is (= [3 4 5.0] (jsv! '(do (defprotocol IPoint
                                   (get-x [this])
                                   (get-y [this])
                                   (distance [this]))
                                 (deftype Point [x y]
                                   IPoint
                                   (get-x [this] (.-x this))
                                   (get-y [this] (.-y this))
                                   (distance [this]
                                     (js/Math.sqrt (+ (* (.-x this) (.-x this))
                                                      (* (.-y this) (.-y this))))))
                                 (def p (Point. 3 4))
                                 [(get-x p) (get-y p) (distance p)])))
        "Multiple protocol methods should handle this correctly")))