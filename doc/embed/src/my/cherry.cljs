(ns my.cherry
  (:require [cherry.embed :as cherry]))

(cherry/preserve-ns 'cljs.core)
(cherry/preserve-ns 'clojure.string)

(defn init []
  (let [[_ expr] (.slice js/process.argv 2)]
    (cherry/eval-string expr)))
