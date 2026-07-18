(ns cherry.dynvars
  "Accessor boxes for cljs.core dynamic vars. Emitted cherry code reads and
  set!s a dynamic var through the box's `.val` (squint's box scheme, legal
  across ESM modules); for core vars the box proxies the real var, so
  bundle-internal readers see mutations."
  #?(:cljs (:require-macros [cherry.dynvars :refer [core-dynvar-boxes]]))
  #?(:clj (:require [clojure.edn :as edn]
                    [clojure.java.io :as io]
                    [clojure.string :as str])))

#?(:clj
   (defn- earmuffed? [sym]
     (let [s (str sym)]
       (and (str/starts-with? s "_STAR_")
            (str/ends-with? s "_STAR_")
            (> (count s) 12)))))

#?(:clj
   (defn- demunge-dynvar
     "_STAR_print_fn_STAR_ -> *print-fn*"
     [sym]
     (let [s (str sym)]
       (symbol (str "*" (-> s
                            (subs 6 (- (count s) 6))
                            (str/replace "_" "-"))
                    "*")))))

#?(:clj
   (defmacro core-dynvar-boxes
     "A def per earmuffed var in cherry/cljs.core.edn, named by the munged
  symbol, holding an accessor box over the real cljs.core var."
     []
     (let [vars (-> (io/resource "cherry/cljs.core.edn") slurp edn/read-string :vars)]
       `(do ~@(for [m vars
                    :when (earmuffed? m)
                    :let [real (symbol "cljs.core" (str (demunge-dynvar m)))]]
                `(def ~(symbol (str m))
                   (accessor-box (fn [] ~real)
                                 (fn [v#] (set! ~real v#)))))))))

#?(:cljs
   (do
     (defn accessor-box [get-fn set-fn]
       (let [o #js {}]
         (js/Object.defineProperty o "val" #js {:get get-fn :set set-fn})
         o))
     (core-dynvar-boxes)))
