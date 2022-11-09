(ns cherry.jsx-test
  (:require
   ["@babel/core" :refer [transformSync]]
   ["react" :as React]
   [cherry.test-utils :refer [jss!]]
   [clojure.test :as t :refer [deftest]]
   [goog.object :as gobject]))

(gobject/set js/global "React" React)

(defn test-jsx [s]
  (let [expr (jss! s)
        code (:code (js->clj (transformSync expr #js {:presets #js ["@babel/preset-react"]}) :keywordize-keys true))]
    (js/eval code)))

(deftest jsx-test
  (test-jsx "#jsx [:a {:href \"http://foo.com\"}]")
  (test-jsx "#jsx [:div #js {:dangerouslySetInnerHTML #js {:_html \"<i>Hello</i>\"}}]")
  (test-jsx "(defn App [] (let [x 1] #jsx [:div {:id x}]))")
  (test-jsx "(let [classes #js {:classes \"foo bar\"}] #jsx [:div {:className (:classes classes)}])")
  (test-jsx "(defn App [^:js {:keys [x]}] #jsx [:span x]) #jsx [App #js {:x 1}]")
  (test-jsx "
(ns foo (:require [\"foo\" :as foo]))
(defn App [] #jsx [foo/c #js {:x 1}]) "))



