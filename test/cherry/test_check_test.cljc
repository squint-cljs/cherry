(ns cherry.test-check-test
  (:require [clojure.test.check.generators :as gen]
            #?@(:clj [[clojure.test.check.properties :as prop]
                      [clojure.test.check.clojure-test :refer [defspec]]]
                :cljs [[clojure.test.check :refer [quick-check]]
                       [clojure.test.check.clojure-test :as tc-test]
                       [clojure.test.check.properties :as prop :refer [for-all*]]
                       [clojure.test :refer [report empty-env set-env! get-current-env
                                             successful? test-var]]]))
  #?(:cljs (:require-macros [clojure.test.check.clojure-test :refer [defspec]]
                            [clojure.test.check.properties :refer [for-all]])))

(defspec simple-integer-property 100
  (prop/for-all [x gen/small-integer]
                (= x x)))

(defspec vector-reversal-property 50
  (prop/for-all [v (gen/vector gen/small-integer)]
                (= v (reverse (reverse v)))))

(defspec generator-composition 50
  (prop/for-all [x (gen/fmap inc gen/small-integer)]
                (> x (- x 1))))

(defspec multiple-generators 50
  (prop/for-all [x gen/small-integer
                 y gen/small-integer]
                (= (+ x y) (+ y x))))

(defspec string-concatenation 50
  (prop/for-all [s1 gen/string-ascii
                 s2 gen/string-ascii]
                (= (count (str s1 s2))
                   (+ (count s1) (count s2)))))

(defspec map-property 50
  (prop/for-all [m (gen/map gen/keyword gen/small-integer)]
                (= (count m) (count (keys m)))))

(defspec set-property 50
  (prop/for-all [s (gen/set gen/small-integer)]
                (<= (count s) 100)))

(defspec such-that-property 50
  (prop/for-all [x (gen/such-that pos? gen/small-integer 100)]
                (pos? x)))

(defspec pos-int-property 50
  (prop/for-all [x (gen/large-integer* {:min 1})]
                (pos? x)))

(defspec neg-int-property 50
  (prop/for-all [x (gen/large-integer* {:max -1})]
                (neg? x)))

(defspec nat-property 50
  (prop/for-all [x gen/nat]
                (nat-int? x)))

(defspec list-distinct-property 50
  (prop/for-all [xs (gen/list-distinct gen/small-integer)]
                (= (count xs) (count (set xs)))))

(defspec recursive-gen-property 20
  (prop/for-all [tree (gen/recursive-gen
                       (fn [inner] (gen/vector inner 0 3))
                       gen/small-integer)]
                (or (number? tree) (vector? tree))))

(defspec resize-property 50
  (prop/for-all [v (gen/resize 5 (gen/vector gen/small-integer))]
                (<= (count v) 10)))

#?(:cljs
   (defn test-clojure-test-vars []
     (assert (number? tc-test/*default-test-count*)
             "*default-test-count* should be a number")))

#?(:cljs
   (defn -main []
     (set-env! (empty-env))
     (test-var simple-integer-property)
     (test-var vector-reversal-property)
     (test-var generator-composition)
     (test-var multiple-generators)
     (test-var string-concatenation)
     (test-var map-property)
     (test-var set-property)
     (test-var such-that-property)
     (test-var pos-int-property)
     (test-var neg-int-property)
     (test-var nat-property)
     (test-var list-distinct-property)
     (test-var recursive-gen-property)
     (test-var resize-property)
     (test-clojure-test-vars)
     (report {:type :summary})
     (let [results (:report-counters (get-current-env))]
       (when-not (successful? results)
         (js/process.exit 1)))))

#?(:cljs (-main))
