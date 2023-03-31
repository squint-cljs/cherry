(ns cherry.embed
  "Experimental."
  (:refer-clojure :exclude [eval])
  (:require [goog.object]
            [cherry.compiler :as cherry])
  (:require-macros [cherry.embed :refer [preserve-ns]]))

(defn ^:no-doc vbk [obj ks]
  (goog.object/getValueByKeys obj ks))

(defn- get+! [o k*]
  (if-some [child-obj (unchecked-get o k*)]
    child-obj
    (unchecked-set o k* #js{})))

(defn ^:no-doc assoc-in!
  "Internal."
  [obj ks* v]
  (let [ks* (mapv name ks*)
        obj (if (some? obj) obj #js{})
        inner-obj (reduce get+! obj (butlast ks*))]
    (unchecked-set inner-obj (peek ks*) v)
    obj))

(comment
  (preserve-ns 'cljs.core))

(defn compile-form
  ([form] (compile-form form nil))
  ([form opts]
   (:body (cherry/compile-form* form
                                (merge {:context :expression
                                        :core-alias 'cljs.core}
                                       opts)))))

(defn eval-form
  ([form] (eval-form form nil))
  ([form opts]
   (js/eval (compile-form form opts))))

(defn compile-string
  ([expr] (compile-string expr nil))
  ([expr opts]
   (:body (cherry/compile-string* expr (merge {:context :expression
                                               :core-alias 'cljs.core}
                                              opts)))))

(defn eval-string
  ([expr] (eval-string expr nil))
  ([expr opts]
   (js/eval (compile-string expr opts))))
