(ns cherry.compiler-test
  (:require
   ["fs" :as fs]
   [cherry.compiler :as cherry]
   [cherry.embed-test]
   [cherry.html-test]
   [cherry.jsx-test]
   [cherry.squint-and-cherry-test]
   [cherry.test-utils :refer [js! jss! jsv!]]
   [clojure.string :as str]
   [clojure.test :as t :refer [async deftest is testing]]))

(def eq =)

(deftest return-test
  (is (str/includes? (jss! '(do (def x (do 1 2 nil))))
                     "return"))
  (is (str/includes? (jss! '(do (def x (do 1 2 "foo"))))
                     "return"))
  (is (str/includes? (jss! '(do (def x (do 1 2 :foo))))
                     "return"))
  (is (str/includes? (jss! "(do (def x (do 1 2 \"hello\")))")
                     "return"))
  (let [s (jss! "(do (def x (do 1 2 [1 2 3])) x)")]
    (is (= [1 2 3] (js/eval s))))
  (let [s (jss! "(do (def x (do 1 2 {:x 1 :y 2})) x)")]
    (is (= {:x 1 :y 2} (js/eval s))))
  (let [s (jss! "(do (def x (do 1 2 #js {:x 1 :y 2})) x)")]
    (is (= (str #js {:x 1 :y 2}) (str (js/eval s))))))

(deftest do-test
  (let [[v s] (js! '(do 1 2 3))]
    (is (= 3 v))
    (is (not (str/includes? s "() =>"))))
  (let [[v s] (js! '(do 1 2 3 (do 4 5 6)))]
    (is (= 6 v))
    (is (not (str/includes? s "() =>"))))
  (let [[v s] (js! '(do (def x (do 4 5 6))
                        x))]
    (is (= 6 v))
    (is (str/includes? s "() =>")))
  (let [[v s] (js! '(let [x (do 4 5 6)]
                      x))]
    (is (= 6 v))
    (is (str/includes? s "() =>"))))

(deftest let-test
  (is (= 3 (jsv! '(let [x (do 1 2 3)] x))))
  (is (= 3 (jsv! '(let [x 1 x (+ x 2)] x))))
  (let [s (jss! '(let [x 1 x (let [x (+ x 1)]
                               x)] x))]
    (is (= 2 (js/eval s))))
  (is (= 7 (jsv! '(let [{:keys [a b]} {:a 1 :b (+ 1 2 3)}]
                    (+ a b)))))
  (is (= 8 (jsv!
            '(+ 1
                (let [{:keys [a b]} {:a 1 :b (+ 1 2 3)}]
                  (+ a b)))))))

(deftest let-interop-test
  (is (= "f" (jsv! '(let [x "foo"]
                      (.substring x 0 1)))))
  (is (= 3 (jsv! '(let [x "foo"]
                    (.-length x))))))

(deftest let-shadow-test
  (is (= 1 (jsv! '(let [name 1]
                    name))))
  (is (= 1 (jsv! '(let [name (fn [] 1)]
                    (name)))))
  (let [s (jss! '(let [name (fn [_] 1)]
                   (map name [1 2 3])))]
    (is (= '(1 1 1)
           (js/eval s))))
  (let [s (jss! '(let [name (fn [_] 1)
                       name (fn [_] 2)]
                   (map name [1 2 3])))]
    (is (= '(2 2 2)
           (js/eval s)))))

(deftest destructure-test
  (let [s (jss! "(let [^js {:keys [a b c]} #js {:a 1 :b 2 :c 3}]
                   (+ a b c))")]
    (is (= 6 (js/eval s)))))

(deftest fn-test
  (let [s (jss! '(let [f (fn [x] x)]
                   f))]
    (is (= 1 ((js/eval s) 1))))
  (let [s (jss! '(let [f (fn [x] 1 2 x)]
                   f))]
    (is (= 1 ((js/eval s) 1))))
  (let [s (jss! '(let [f (fn [x] 1 2 (+ 1 x))]
                   f))]
    (is (= 2 ((js/eval s) 1))))
  (let [s (jss! '(let [f (fn [x] 1 2 (do 1 x))]
                   f))]
    (is (= 1 ((js/eval s) 1))))
  (is (= 1 (jsv! '(do (defn foo [] (fn [x] x)) ((foo) 1))))))

(deftest fn-varargs-test
  (doseq [repl [true false]]
    (testing "vararg fixed arity"
      (is (nil? (jsv! '(let [f (fn foo [x y & zs] zs)] (f 1 2)) {:repl repl}))))
    (testing "vararg vararg arity"
      (is (= [3 4] (jsv! '(let [f (fn foo [x y & zs] zs)] (f 1 2 3 4)) {:repl repl}))))
    (testing "multi vararg fixed arity"
      (is (= 1 (jsv! '(let [f (fn foo
                                 ([y] 1)
                                 ([y & zs] zs))]
                         (f 1))
                      {:repl repl}))))
    (testing "multi vararg vararg arity"
      (is (= [2] (jsv! '(let [f (fn foo
                                  ([y] 1)
                                  ([y & zs] zs))]
                          (f 1 2)) {:repl repl}))))))

(deftest fn-multi-arity-test
  (is (= 1 (jsv! '(let [f (fn foo ([x] x) ([x y] y))] (f 1)))))
  (is (= 2 (jsv! '(let [f (fn foo ([x] x) ([x y] y))] (f 1 2)))))
  (is (= 2 (jsv! '(let [f (fn foo-bar ([x] x) ([x y] y))] (f 1 2))))))

(deftest fn-multi-varargs-test
  (is (= 1 (jsv! '(let [f (fn foo ([x] x) ([x y & zs] zs))] (f 1)))))
  (is (= '(3 4) (jsv! '(let [f (fn foo ([x] x) ([x y & zs] zs))] (f 1 2 3 4)))))
  (is (nil? (jsv! '(let [f (fn foo ([x] x) ([x y & zs] zs))] (f 1 2))))))

(deftest defn-test
  (let [s (jss! '(do (defn f [x] x) f))]
    (is (= 1 ((js/eval s) 1))))
  (let [s (jss! '(do (defn f [x] (let [y 1] (+ x y))) f))]
    (is (= 2 ((js/eval s) 1))))
  (let [s (jss! '(do (defn foo [x]
                       (dissoc x :foo))
                     (foo {:a 1 :foo :bar})))]
    (is (= {:a 1} (js/eval s))))
  (let [s (jss! "(do (defn f [^js {:keys [a b c]}] (+ a b c)) f)")]
    (is (= 6 ((js/eval s) #js {:a 1 :b 2 :c 3}))))
  (let [s (jss! '(do (defn quux [x]
                       (if (pos? x)
                         1
                         2))
                     (quux 1)))]
    (is (= 1 (js/eval s)))))

(deftest defn-multi-arity-test
  (is (= 1 (jsv! '(do
                    (defn foo ([x] x) ([x y] y))
                    (foo 1)))))
  (is (= 2 (jsv! '(do
                    (defn foo ([x] 1) ([x y] y))
                    (foo 1 2))))))

(deftest defn-recur-test
  (let [s (jss! '(do (defn quux [x]
                       (if (pos? x)
                         (recur (dec x))
                         x))
                     (quux 1)))]
    (is (zero? (js/eval s)))))

(deftest defn-varargs-test
  (let [s (jss! '(do (defn foo [x & args] args) (foo 1 2 3)))]
    (is (= '(2 3) (js/eval s)))))

(deftest defn-multi-varargs-test
  (is (= [1 [1 2 '(3 4)]]
         (js/eval
          (jss! '(do (defn foo
                       ([x] x)
                       ([x y & args]
                        [x y args]))
                     [(foo 1) (foo 1 2 3 4)]))))))

(deftest loop-test
  (let [s (jss! '(loop [x 1] (+ 1 2 x)))]
    (is (= 4 (js/eval s))))
  (let [s (jss! '(loop [x 10]
                   (if (pos? x)
                     (recur (dec x))
                     x)))]
    (is (zero? (js/eval s)))))

(deftest if-test
  (is (true? (jsv! "(if 0 true false)")))
  (let [s (jss! "[(if false true false)]")]
    (false? (first (js/eval s))))
  (let [s (jss! "(let [x (if (inc 1) (inc 2) (inc 3))]
                   x)")]
    (is (= 3 (js/eval s))))
  (let [s (jss! "(let [x (do 1 (if (inc 1) (inc 2) (inc 3)))]
                   x)")]
    (is (= 3 (js/eval s)))))

(deftest doseq-test
  (let [s (jss! '(let [a (atom [])]
                   (doseq [x [1 2 3]]
                     (swap! a conj x))
                   (deref a)))]
    (is (= [1 2 3] (js/eval s))))
  (let [s (jss! '(let [a (atom [])]
                   (doseq [x [1 2 3]
                           y [4 5 6]]
                     (swap! a conj x y))
                   (deref a)))]
    (is (= [1 4 1 5 1 6 2 4 2 5 2 6 3 4 3 5 3 6]
           (js/eval s)))))

(deftest for-test
  (let [s (jss! '(for [x [1 2 3] y [4 5 6]] [x y]))]
    (is (= '([1 4] [1 5] [1 6] [2 4] [2 5] [2 6] [3 4] [3 5] [3 6])
           (js/eval s)))))

(deftest regex-test
  (is (= '("foo" "foo")
         (jsv! '(re-seq #"foo" "foo foo")))))

(deftest new-test
  (is (= "hello" (jsv! '(str (js/String. "hello"))))))

(deftest quote-test
  (is (= '{x 1} (jsv! (list 'quote '{x 1}))))
  (is (= '(def x 1) (jsv! (list 'quote '(def x 1))))))

(deftest case-test
  (let [s (jss! '(case 'x x 2))]
    (is (= 2 (js/eval s))))
  (is (= 2 (jsv! '(case 1 1 2 3 4))))
  (is (= 5 (jsv! '(case 6 1 2 3 4 (inc 4)))))
  (is (= 2 (jsv! '(case 1 :foo :bar 1 2))))
  (is (= :bar (jsv! '(case :foo :foo :bar))))
  (is (thrown-with-msg? js/Error #"No matching clause"
                        (jsv! '(case 'x y 2))))
  (let [s (jss! '(let [x (case 1 1 2 3 4)]
                   (inc x)))]
    (is (= 3 (js/eval s))))
  (let [s (jss! '(do (defn foo []
                       (case 1 1 2 3 4))
                     (foo)))]
    (is (= 2 (js/eval s)))))

(deftest dot-test
  (let [s (jss! "(do (def x (.-x #js {:x 1})) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(do (def x (. #js {:x 1} -x)) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(do (def x (.x #js {:x (fn [] 1)})) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(do (def x (.x #js {:x (fn [x] x)} 1)) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(do (def x (. #js {:x (fn [x] x)} x 1)) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(do (def x (. #js {:x (fn [x] x)} (x 1))) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(.goto #js {:goto (fn [x] [:hello x])} 10)")]
    (is (= [:hello 10] (js/eval s)))))

(deftest dotdot-test
  (let [s (jss! "(.. #js {:foo #js {:bar 2}} -foo -bar)")]
    (is (= 2 (js/eval s)))))

#_(js-delete js/require.cache (js/require.resolve "/tmp/debug.js"))
#_(js/require "/tmp/debug.js")

(deftest backtick-test
  (is (= '(assoc {} :foo :bar) (jsv! "`(assoc {} :foo :bar)"))))

(deftest munged-core-name-test
  (is (jsv! '(boolean 1))))

(deftest defprotocol-extend-type-string-test
  (is (= :foo (jsv! '(do (defprotocol IFoo (foo [_] "docstring"))
                         (extend-type string IFoo (foo [_] :foo))
                          (foo "bar"))))))

(deftest deftype-test
  (is (= 1 (jsv! '(do (deftype Foo [x]) (.-x (->Foo 1))))))
  (is (= [:foo :bar]
         (jsv! '(do
                  (defprotocol IFoo (foo [_]) (bar [_]))
                  (deftype Foo [x] IFoo (foo [_] :foo)
                           (bar [_] :bar))
                  (let [x (->Foo 1)]
                    [(foo x) (bar x)])))))
  (is (= [:foo 2]
         (jsv! '(do (defprotocol IFoo (foo [_]) (bar [_]))
                    (deftype Foo [^:mutable x]
                      IFoo
                      (foo [_] [:foo x])
                      (bar [_] :bar))
                    (def x (->Foo 1))
                    (set! (.-x x) 2)
                    (foo x)))))
  (is (= [:foo 2]
         (jsv! "(do (deftype Foo [^:mutable x]
                      Object
                      (getValue [this] (.-x this)))
                    (def x (->Foo [:foo 2]))
                    (.getValue x))"))))

(deftest set-test
  (is (= #{1 2 3 4 5 6} (jsv! '(into #{1 2 3} #{4 5 6})))))

(deftest await-test
  (async done
         (->
          (.then (jsv! '(do (defn ^:async foo []
                              (js/await (js/Promise.resolve :hello)))

                            (defn ^:async bar []
                              (let [x (js/await (foo))]
                                x))

                            (bar)))
                 (fn [v]
                   (is (= :hello v))))
          (.catch (fn [err]
                    (is false (.-message err))))
          (.finally #(done)))))

(deftest await-variadic-test
  (async done
         (->
          (.then (jsv! '(do (defn ^:async foo [& xs] (js/await 10))
                            (defn ^:async bar [x & xs] (js/await 20))
                            (defn ^:async baz
                              ([x] (baz x 1 2 3))
                              ([x & xs]
                               (let [x (js/await (foo x))
                                     y (js/await (apply bar xs))]
                                 (+ x y))))

                            (baz 1)))
                 (fn [v]
                   (is (= 30 v))))
          (.catch (fn [err]
                    (is false (.-message err))))
          (.finally #(done)))))

(deftest native-js-array-test
  (let [s (jss! "(let [x 2
                       x #js [1 2 x]]
                   x)")
        x (js/eval s)]
    (is (array? x))
    (is (= [1 2 2] (js->clj x))))
  (is (= 1 (jsv! "(aget  #js [1 2 3] 0)"))))

(deftest keyword-call-test
  (is (= :bar (jsv! '(:foo {:foo :bar}))))
  (is (= :bar (jsv! '(let [x :foo]
                       (x {:foo :bar})))))
  (is (= :bar (jsv! '((keyword "foo") {:foo :bar})))))

(deftest minus-single-arg-test
  (is (= -10 (jsv! '(- 10))))
  (is (= -11 (jsv! '(- 10 21)))))

(deftest namespace-keywords
  (is (= :hello/world (jsv! "(ns hello) ::world"))))

(deftest plus-hof-test
  (is (= 6 (jsv! "(apply + [1 2 3])"))))

(deftest instance?-test
  (is (jsv! '(do (deftype Foo []) (instance? Foo (->Foo))))))

(deftest logic-return
  (is (= 2 (jsv! '(do (defn foo [a b] (and a b)) (foo 1 2)))))
  (is (= 1 (jsv! '(do (defn foo [a b] (or a b)) (foo 1 2))))))

(deftest multiple-arity-infix
  (is (true? (jsv! '(> 5 4 3 2 1))))
  (is (true? (jsv! '(> 5 4 3))))
  (is (true? (jsv! '(> 5 4))))
  ;; I would say this is undefined in clava for now:
  #_(is (true? (jsv! '(> 5)))))

(deftest equality-test
  (is (false? (jsv! "(= 1 2)")))
  (is (true? (jsv! "(= 1 1)")))
  (is (true? (jsv! "(= [1 2 3] [1 2 3])"))))

(deftest list-test
  (testing "creates a list of elements"
    (is (eq '(1 2 3) (jsv! '(list 1 2 3)))))
  (testing "accepts a single, numeric element in the list"
    (is (eq '(23) (jsv! '(list 23)))))
  (testing "creates an empty list"
    (is (eq '() (jsv! '(list))))
    (is (eq '() (jsv! "()")))
    (is (eq '() (jsv! "'()")))
    (is (eq '(1 2 3) (jsv! "'(1 2 3)")))))

(deftest double-names-in-sig-test
  (is (= 2 (jsv! '(do (defn foo [x x] x) (foo 1 2))))))

(deftest try-catch-test
  (is (= 2 (jsv! '(try (assoc :foo 1 2) (catch :default _ 2))))))

(deftest require-with-kebab-case-alias-test
  (let [s (cherry/compile-string "(ns test-namespace (:require [\"some-js-library$default\" :as some-js-lib])) (some-js-lib/some_fn)")]
    (is (str/includes? s "import some_js_lib from 'some-js-library';"))
    (is (str/includes? s "some_js_lib.some_fn.call(null);"))
    (is (not (str/includes? s "import * as some_js_lib"))))

  (let [s (cherry/compile-string "(ns test-namespace (:require [\"some-js-library\" :as some-js-lib])) (some-js-lib/some_fn)")]
    (is (str/includes? s "import * as some_js_lib from 'some-js-library'"))
    (is (str/includes? s "some_js_lib.some_fn.call(null);")))

  (let [s (cherry/compile-string "(ns test-namespace (:require [\"./local_file.mjs\" :as local-file])) (local-file/some_fn)")]
    (is (str/includes? s "import * as local_file from './local_file.mjs'"))
    (is (str/includes? s "local_file.some_fn.call(null);")))
  (let [s (cherry/compile-string "(ns bar (:require [\"./foo.mjs\" #_#_:refer [foo-bar] :as foo-alias])) (prn foo-alias/foo-bar)")]
    (is (str/includes? s "import * as foo_alias from './foo.mjs'"))
    (is (str/includes? s "prn.call(null, foo_alias.foo_bar);"))))

(deftest zero?-test
  (is (str/includes? (jss! "(if (zero? x) 1 2)") "== 0"))
  (is (not (str/includes? (jss! "(if (zero? x) 1 2)") "truth_"))))

(deftest with-out-str-test
  (is (str/includes? (jsv! "(with-out-str (time :hello))") "Elapsed time")))

(deftest defclass-test
  (is (= "[\"<<<<1-3-3>>>>\" \"1-3-3\"]" (str (jsv! (str (fs/readFileSync "test-resources/defclass_test.cljs")))))))

(deftest override-core-var-test
  (is (= 1 (jsv! "(def count 1) (set! count (inc count)) (defn frequencies [x] (dec x)) (frequencies count)"))))

(deftest no-truth-check-test
  (let [inputs ["(if (zero? 0) 1 2)" "(when (< 1 2) 1)" ;; "(when (= 1 1) 1)"
                "(let [x (zero? 0)] (when x 1))"
                "(if (neg? 1) 0 1)" ;; "(if (not 1) 0 1)"
                "(if \"foo\" 1 2)" ;; "(if :foo 1 2)"
                "(let [x nil] (if (nil? x) 1 2))"
                "(let [x (zero? 0) y x] (if y 1 2))"
                "(if (coercive-boolean (range 0 1)) 1 2)"]]
    (doseq [input inputs]
      (let [js (jss! input)]
        (is (not (str/includes? js "truth_")) (str "contains truth check: " input "\n" js))
        (is (= 1 (js/eval js)))))))

(deftest map-literal-test
  (is (= {} (jsv! '{})))
  (is (= {1 true} (jsv! '(do (def x 1) {x true}))))
  (is (= {[0,1] true} (jsv! '{[0 1] true}))))

(deftest aset-test
  (is (= [1] (js->clj (jsv! "(def x #js []) (aset x 0 1) x"))))
  (testing "multiple dimensions"
    (is (= [[1]] (js->clj (jsv! "(def x #js [#js []]) (aset x 0 0 1) x"))))
    (is (= [[0 1]] (js->clj (jsv! "(def x #js [#js [0]]) (aset x 0 1 1) x")))))
  (testing "emit direct array access"
    (let [js (jss! "(aset #js [#js [0]] 0 0 :hello)")]
      (is (not (str/includes? js "aset")))
      (is (str/includes? js "[[0]][0][0] ="))
      (is (eq :hello (js/eval js))))))

(deftest Math-test
  (let [expr '(Math/sqrt 3.14)]
    (is (= (Math/sqrt 3.14) (jsv! (str expr))))
    (testing "repl-mode"
      (let [s (jss! (str expr) {:repl true})]
        (is (str/includes? s "globalThis.user"))
        (is (not (str/includes? s "globalThis.user.Math")))
        (is (= (Math/sqrt 3.14) (js/eval s)))))))

(defn wrap-async [s]
  (str/replace "(async function () {\n%s\n})()" "%s" s))

(deftest set-lib-test
  (t/async done
    (let [js (cherry/compile-string "(ns foo (:require [clojure.set :as set]))
             [(set/intersection #{:a :b})
              (set/intersection #{:a :b} #{:b :c})]" {:repl true
                                                      :context :return})]
      (-> (.then (js/eval (wrap-async js))
                 (fn [vs]
                   (let [expected [#{:a :b} #{:b}]
                         pairs (map vector expected vs)]
                     (doseq [[expected s] pairs]
                       (is (eq expected s))))))
          (.finally done)))))

(deftest built-in-protocol-test
  (is (= 8 (jsv! "(deftype Signal [x]
IDeref (-deref [this] x)
ISwap (-swap! [this f]
              (set! x (f x))
              x)
      (-swap! [this f a1]
              (set! x (f x a1))
              x)
IReset (-reset! [this v]
        (set! x v)
        x))

(def x (Signal. 1))
(reset! x (inc @x))
(swap! x inc)
(swap! x (fn [old-val v] (+ old-val v)) 5)

(deref x)"))))

(deftest pragmas-test
  (let [code "\"use client\"
(js* \"// ts-check\")
(js* \"'use server'\")
(js* \"/**
* @param {number} x
*/\")
(defn foo [x] (merge x nil))
\"use serverless\"
"]
    (doseq [code [code (str/replace "(do %s)" "%s" code)]
            repl? [true false]
            return? [true false]]
      (let [{:keys [pragmas javascript]} (cherry/compile-string* code {:repl repl?
                                                                       :context (if return?
                                                                                  :return
                                                                                  :statement)})]
        (is (str/includes? pragmas "use client"))
        (is (str/includes? pragmas "// ts-check"))
        (is (not (str/includes? pragmas ";")))
        (is (< (str/index-of javascript "use client")
               (str/index-of javascript "ts-check")
               (str/index-of javascript "'use server'")
               (str/index-of javascript "import")
               (str/index-of javascript "@param")
               (str/index-of javascript "foo = function")
               (str/index-of javascript "use serverless")))))))

(deftest not=-test
  (is (false? (jsv! "(not= {:a 1} {:a 1})"))))

(deftest macro-state-test
  (let [js-opts (clj->js {:macros {'foo.bar {'mymacro (fn [_ _ x] `(+ ~x ~x))}}
                          :context :expr})
        state (cherry/compileStringEx "(ns foo (:require [foo.bar :refer [mymacro]]))" js-opts nil)
        state (cherry/compileStringEx "(mymacro (+ 1 2 3))" js-opts state)
        body (aget state "body")]
    (is (= 12 (js/eval body)))))

(deftest assert-test
  (testing "assert with true condition does not throw"
    (is (= 42 (jsv! "(do (assert true) 42)"))))
  (testing "assert with false condition throws"
    (is (thrown-with-msg? js/Error #"Assert failed"
                          (jsv! "(assert false)"))))
  (testing "assert with false and message throws with message"
    (is (thrown-with-msg? js/Error #"custom message"
                          (jsv! "(assert false \"custom message\")")))))

(deftest satisfies?-test
  (testing "satisfies? returns true for implemented custom protocol"
    (is (true? (jsv! "(defprotocol IFoo (-foo [_])) (deftype Bar [] IFoo (-foo [_] 42)) (satisfies? IFoo (Bar.))"))))
  (testing "satisfies? returns false for non-implemented custom protocol"
    (is (false? (jsv! "(defprotocol IFoo (-foo [_])) (deftype Bar []) (satisfies? IFoo (Bar.))"))))
  (testing "satisfies? returns true for ClojureScript core protocol"
    (is (true? (jsv! "(deftype T [] ICounted (-count [_] 42)) (satisfies? ICounted (T.))"))))
  (testing "satisfies? returns false for non-implemented ClojureScript protocol"
    (is (false? (jsv! "(deftype T []) (satisfies? ICounted (T.))")))))

(deftest protocol-dispatch-test
  (testing "ICounted" (is (= 42 (jsv! "(deftype T [] ICounted (-count [_] 42)) (count (T.))"))))
  (testing "IEmptyableCollection" (is (= 0 (jsv! "(deftype T [] IEmptyableCollection (-empty [_] []) ICounted (-count [_] 0)) (count (empty (T.)))"))))
  (testing "ICollection" (is (= 1 (jsv! "(deftype T [x] ICollection (-conj [_ v] v)) (conj (T. nil) 1)"))))
  (testing "IIndexed" (is (= :a (jsv! "(deftype T [] IIndexed (-nth [_ i] :a) (-nth [_ i nf] :a)) (nth (T.) 0)"))))
  (testing "ISeq" (is (= 1 (jsv! "(deftype T [] ISeq (-first [_] 1) (-rest [_] nil)) (first (T.))"))))
  (testing "INext" (is (nil? (jsv! "(deftype T [] INext (-next [_] nil)) (next (T.))"))))
  (testing "ILookup" (is (= :v (jsv! "(deftype T [] ILookup (-lookup [_ k] :v) (-lookup [_ k nf] :v)) (get (T.) :k)"))))
  (testing "IAssociative" (is (true? (jsv! "(deftype T [] IAssociative (-contains-key? [_ k] true) (-assoc [this k v] this)) (contains? (T.) :k)"))))
  (testing "IMap" (is (jsv! "(deftype T [x] IMap (-dissoc [this k] this)) (dissoc (T. 1) :k)")))
  (testing "IMapEntry" (is (= :k (jsv! "(deftype T [] IMapEntry (-key [_] :k) (-val [_] :v)) (key (T.))"))))
  (testing "ISet" (is (jsv! "(deftype T [x] ISet (-disjoin [this v] this)) (disj (T. 1) :x)")))
  (testing "IStack" (is (= 1 (jsv! "(deftype T [] IStack (-peek [_] 1) (-pop [this] this)) (peek (T.))"))))
  (testing "IVector" (is (jsv! "(deftype T [x] IAssociative (-contains-key? [_ k] false) (-assoc [this k v] this) IVector (-assoc-n [this i v] this)) (assoc (T. []) 0 :v)")))
  (testing "IDeref" (is (= 99 (jsv! "(deftype T [v] IDeref (-deref [_] v)) (deref (T. 99))"))))
  (testing "IMeta" (is (= {:a 1} (jsv! "(deftype T [] IMeta (-meta [_] {:a 1})) (meta (T.))"))))
  (testing "IWithMeta" (is (jsv! "(deftype T [] IWithMeta (-with-meta [this m] this)) (with-meta (T.) {:a 1})")))
  (testing "IReduce" (is (= 10 (jsv! "(deftype T [] IReduce (-reduce [_ f] (f 10)) (-reduce [_ f init] (f init 10))) (reduce + (T.))"))))
  (testing "IKVReduce" (is (= 5 (jsv! "(deftype T [] IKVReduce (-kv-reduce [_ f init] (f init :k :v))) (reduce-kv (fn [a k v] 5) 0 (T.))"))))
  (testing "IEquiv" (is (true? (jsv! "(deftype T [] IEquiv (-equiv [_ o] true)) (= (T.) :x)"))))
  (testing "IHash" (is (= 42 (jsv! "(deftype T [] IHash (-hash [_] 42)) (hash (T.))"))))
  (testing "ISeqable" (is (= 1 (jsv! "(deftype T [] ISeqable (-seq [_] [1])) (first (seq (T.)))"))))
  (testing "IReversible" (is (= [1] (jsv! "(deftype T [] IReversible (-rseq [_] [1])) (rseq (T.))"))))
  (testing "IPrintWithWriter" (is (jsv! "(deftype T [] IPrintWithWriter (-pr-writer [_ writer opts] nil)) (pr-str (T.))")))
  (testing "IPending" (is (true? (jsv! "(deftype T [] IPending (-realized? [_] true)) (realized? (T.))"))))
  (testing "IWatchable" (is (jsv! "(deftype T [] IWatchable (-add-watch [this k f] this) (-remove-watch [this k] this) (-notify-watches [_ oldval newval] nil)) (add-watch (T.) :k (fn [_ _ _ _]))")))
  (testing "IEditableCollection" (is (jsv! "(deftype T [] IEditableCollection (-as-transient [this] this)) (transient (T.))")))
  (testing "ITransientCollection" (is (jsv! "(deftype T [] ITransientCollection (-conj! [this v] this) (-persistent! [this] [])) (persistent! (T.))")))
  (testing "ITransientAssociative" (is (jsv! "(deftype T [] ITransientAssociative (-assoc! [this k v] this)) (assoc! (T.) :k :v)")))
  (testing "ITransientMap" (is (jsv! "(deftype T [] ITransientMap (-dissoc! [this k] this)) (dissoc! (T.) :k)")))
  (testing "ITransientVector" (is (jsv! "(deftype T [] ITransientAssociative (-assoc! [this k v] this) ITransientVector (-assoc-n! [this i v] this) (-pop! [this] this)) (assoc! (T.) 0 :v)")))
  (testing "ITransientSet" (is (jsv! "(deftype T [] ITransientSet (-disjoin! [this v] this)) (disj! (T.) :v)")))
  (testing "IComparable" (is (= 0 (jsv! "(deftype T [] IComparable (-compare [_ other] 0)) (compare (T.) :x)"))))
  (testing "INamed" (is (= "foo" (jsv! "(deftype T [] INamed (-name [_] \"foo\") (-namespace [_] nil)) (name (T.))"))))
  (testing "ICloneable" (is (jsv! "(deftype T [x] ICloneable (-clone [_] (T. x))) (clone (T. 1))")))
  (testing "IReset" (is (= 5 (jsv! "(deftype T [a] IReset (-reset! [_ v] (reset! a v))) (reset! (T. (atom 0)) 5)"))))
  (testing "ISwap" (is (= 2 (jsv! "(deftype T [a] ISwap (-swap! [_ f] (swap! a f)) (-swap! [_ f x] (swap! a f x)) (-swap! [_ f x y] (swap! a f x y)) (-swap! [_ f x y z] (swap! a f x y z))) (swap! (T. (atom 1)) inc)")))))

(defn init []
  (cljs.test/run-tests 'cherry.compiler-test 'cherry.jsx-test 'cherry.squint-and-cherry-test
                       'cherry.html-test 'cherry.embed-test))
