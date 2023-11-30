(ns cherry.jsx-test
  (:require
   ["@babel/core" :refer [transformSync]]
   ["react" :as React]
   [cherry.test-utils :refer [jss!]]
   [clojure.test :as t :refer [deftest is testing]]
   [goog.object :as gobject]
   [clojure.string :as str]))

(gobject/set js/global "React" React)

(defn test-jsx [s]
  (let [expr (jss! s)
        code (:code (js->clj (transformSync expr #js {:presets #js ["@babel/preset-react"]}) :keywordize-keys true))]
    (js/eval code)))

(deftest jsx-test
  (test-jsx "#jsx [:a {:href \"http://foo.com\"}]")
  (test-jsx "#jsx [:div {:dangerouslySetInnerHTML {:_html \"<i>Hello</i>\"}}]")
  (test-jsx "(defn App [] (let [x 1] #jsx [:div {:id x}]))")
  (test-jsx "(let [classes {:classes \"foo bar\"}] #jsx [:div {:className (:classes classes)}])")
  (test-jsx "(defn App [{:keys [x]}] #jsx [:span x]) #jsx [App {:x 1}]")
  (test-jsx "
(ns foo (:require [\"foo\" :as foo]))
(defn App [] #jsx [foo/c #js {:x 1}]) ")
  (test-jsx "(defn TextField [{:keys [multiline]}]) #jsx [TextField {:multiline true}]")
  (testing "spread"
    (test-jsx "(defn TextField [{:keys [multiline]}]) (let [m {:a 1}] #jsx [TextField {:foo 1 :& m}])")
    (test-jsx "(defn TextField [{:keys [multiline]}]) (let [m {:a 1}] #jsx [TextField {:& m :foo :bar}])")
    (test-jsx "(defn TextField []) #jsx [TextField {:styles [1 2 3]}]"))
  (let [s (jss! "#jsx [:View [:Text \"Hello\"][:Text \"World! \"]]")]
    (is (str/includes? s "</Text><Text>"))
    (is (str/includes? s "<Text>World! </Text>"))))



