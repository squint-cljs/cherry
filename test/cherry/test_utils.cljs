(ns cherry.test-utils
  (:require
   ["cherry-cljs/cljs.core.js" :as cl]
   [cherry.compiler :as cherry]
   [clojure.test :as t]))

(def old-fail (get-method t/report [:cljs.test/default :fail]))

(defmethod t/report [:cljs.test/default :fail] [m]
  (set! js/process.exitCode 1)
  (old-fail m))

(defmethod t/report [:cljs.test/default :begin-test-var] [m]
  (let [var (:var m)
        name (:name (meta var))]
    (println "====" name)))

(def old-error (get-method t/report [:cljs.test/default :fail]))

(defmethod t/report [:cljs.test/default :error] [m]
  (set! js/process.exitCode 1)
  (old-error m))

(doseq [k (js/Object.keys cl)]
  (aset js/globalThis k (aget cl k)))

(defn jss!
  ([expr] (jss! expr nil))
  ([expr opts]
   (if (string? expr)
     (:body (cherry/compile-string* expr (merge {:elide-imports true
                                                 :core-alias nil}
                                                opts)))
     (cherry/transpile-form-internal expr (merge {:elide-imports true
                                                  :core-alias nil}
                                                 opts)))))

(defn js!
  ([expr] (js! expr nil))
  ([expr opts]
   (let [js (jss! expr opts)]
     [(js/eval js) js])))

(defn jsv!
  ([expr] (jsv! expr nil))
  ([expr opts]
   (first (js! expr opts))))

