(ns cherry.embed
  (:require [cljs.analyzer.api :as ana]))

(defmacro preserve-ns [[_quote sym]]
  (let [ns-sym (gensym "ns")
        ns-str (str sym)
        path-sym (symbol "path")]
    `(let [~path-sym (.split ~ns-str ".")
           ~ns-sym (cherry.embed/vbk js/globalThis ~path-sym)]
       (let [~ns-sym (or ~ns-sym
                         (let [obj# (cljs.core/js-obj)]
                           (cherry.embed/assoc-in! js/globalThis ~path-sym obj#)
                           obj#))]
         ~@(keep (fn [[k v]]
                   (if (or (:fn-var v)
                           (:type v))
                     `(~'unchecked-set ~ns-sym
                       (str (munge ~(str k))) ~(symbol (str sym) (str k)))
                     nil #_(binding [*out* *err*]
                             (prn v))))
                 (ana/ns-publics sym))))))
