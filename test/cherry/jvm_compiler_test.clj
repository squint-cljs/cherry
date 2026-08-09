(ns cherry.jvm-compiler-test
  (:require [cherry.compiler :as cherry]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(deftest js-reserved-word-test
  ;; clojure.core/munge leaves JS reserved words alone, cljs.core/munge appends $
  (is (str/includes? (cherry/compile-string "(defn f [new] new)") "new$"))
  (is (str/includes? (cherry/compile-string "(deftype T [new] Object (toString [_] new))")
                     "this.new$"))
  (is (str/includes? (cherry/compile-string "(defprotocol P (delete [x]))")
                     "P$delete$$arity$1"))
  (is (str/includes?
       (cherry/compile-string "(require '[cherry.core :refer [defclass]])
                               (defclass MyElement (extends js/HTMLElement) (constructor [this] (super)))")
       "const this$ = this;")))
