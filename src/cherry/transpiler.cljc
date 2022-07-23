;;; scriptjure -- a library for generating javascript from Clojure s-exprs

;; by Allen Rohner, http://arohner.blogspot.com
;;                  http://www.reasonr.com
;; October 7, 2009

;; Copyright (c) Allen Rohner, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; This library generates javascript from Clojure s-exprs. To use it,
;; (js (fn foo [x] (var x (+ 3 5)) (return x)))
;;  returns a string, "function foo (x) { var x = (3 + 5); return x; }"
;;
;; See the README and the tests for more information on what is supported.
;;
;; The library is intended to generate javascript glue code in Clojure
;; webapps. One day it might become useful enough to write entirely
;; JS libraries in clojure, but it's not there yet.
;;
;;

(ns #^{:author "Allen Rohner"
       :doc "A library for generating javascript from Clojure."}
    cherry.transpiler
  (:require
   #?(:cljs ["fs" :as fs])
   #?(:cljs [goog.string.format])
   #?(:cljs [goog.string :as gstring])
   #?(:clj [cherry.resource :as resource])
   [cherry.internal.destructure :refer [core-let]]
   [cherry.internal.fn :refer [core-defn core-fn]]
   [cherry.internal.loop :as loop]
   [cherry.internal.macros :as macros]
   [clojure.string :as str]
   [com.reasonr.string :as rstr]
   [edamame.core :as e])
  #?(:cljs (:require-macros [cherry.resource :as resource])))

#?(:cljs (def Exception js/Error))

#?(:cljs (def format gstring/format))

(defmulti emit (fn [expr _ctx] (type expr)))

(defmulti emit-special (fn [disp _ctx & _args] disp))

(def statement-separator ";\n")

;; TODO: move to context argument
(def ^:dynamic *async* false)
(def ^:dynamic *imported-core-vars* (atom #{}))
(def ^:dynamic *public-vars* (atom #{}))

(defn statement [expr]
  (if (not (= statement-separator (rstr/tail (count statement-separator) expr)))
    (str expr statement-separator)
    expr))

(defn comma-list [coll]
  (str "(" (str/join ", " coll) ")"))

(defmethod emit nil [_ _]
  "null")

(defmethod emit #?(:clj java.lang.Integer :cljs js/Number) [expr _ctx]
  (str expr))

#?(:clj (defmethod emit clojure.lang.Ratio [expr]
          (str (float expr))))

(defmethod emit #?(:clj java.lang.String :cljs js/String) [^String expr _ctx]
  (pr-str expr))

(defmethod emit #?(:clj clojure.lang.Keyword :cljs Keyword) [expr _ctx]
  (swap! *imported-core-vars* conj 'keyword)
  (str (format "keyword(%s)" (pr-str (subs (str expr) 1)))))

(defn munge* [expr]
  (let [munged (str (munge expr))]
    (cond-> munged
      (and (str/ends-with? munged "$")
           (not= (str expr) "default"))
      (str/replace #"\$$" ""))))

(defmethod emit #?(:clj clojure.lang.Symbol :cljs Symbol) [expr _ctx]
  (let [expr-ns (namespace expr)
        js? (= "js" expr-ns)
        expr-ns (when-not js? expr-ns)
        expr (str expr-ns (when expr-ns
                            ".")
                  (munge* (name expr)))]
    (str expr)))

#?(:clj (defmethod emit #?(:clj java.util.regex.Pattern) [expr _ctx]
          (str \/ expr \/)))

(defmethod emit :default [expr _ctx]
  ;; RegExp case moved here:
  ;; References to the global RegExp object prevents optimization of regular expressions.
  #?(:cljs (if (instance? js/RegExp expr)
             (str \/ expr \/)
             (str expr))
     :clj (str expr)))

(def special-forms (set ['var '. '.. 'if 'funcall 'fn 'fn* 'quote 'set!
                         'return 'delete 'new 'do 'aget 'while 'doseq
                         'inc! 'dec! 'dec 'inc 'defined? 'and 'or
                         '? 'try 'break
                         'await 'const 'defn 'let 'let* 'ns 'def 'loop*
                         'recur]))

(def built-in-macros {'-> macros/core->
                      '->> macros/core->>
                      'as-> macros/core-as->
                      'comment macros/core-comment
                      'dotimes macros/core-dotimes
                      'if-not macros/core-if-not
                      'when macros/core-when
                      'when-not macros/core-when-not
                      'doto macros/core-doto
                      'cond macros/core-cond
                      'cond-> macros/core-cond->
                      'cond->> macros/core-cond->>
                      'if-let macros/core-if-let
                      'if-some macros/core-if-some
                      'when-let macros/core-when-let
                      'when-first macros/core-when-first
                      'when-some macros/core-when-some
                      'some-> macros/core-some->
                      'some>> macros/core-some->>
                      'loop loop/core-loop})

(def core-config (resource/edn-resource "cherry/cljs.core.edn"))

(def core-vars (:vars core-config))

(def prefix-unary-operators (set ['!]))

(def suffix-unary-operators (set ['++ '--]))

(def infix-operators (set ['+ '+= '- '-= '/ '* '% '== '=== '< '> '<= '>= '!=
                           '<< '>> '<<< '>>> '!== '& '| '&& '|| '= 'not= 'instanceof]))

(def chainable-infix-operators (set ['+ '- '* '/ '& '| '&& '||]))


(defn special-form? [expr]
  (contains? special-forms expr))


(defn infix-operator? [expr]
  (contains? infix-operators expr))

(defn prefix-unary? [expr]
  (contains? prefix-unary-operators expr))

(defn suffix-unary? [expr]
  (contains? suffix-unary-operators expr))

(defn emit-prefix-unary [_type [operator arg]]
  (str operator (emit arg)))

(defn emit-suffix-unary [_type [operator arg]]
  (str (emit arg) operator))

(defn emit-infix [_type ctx [operator & args]]
  (when (and (not (chainable-infix-operators operator)) (> (count args) 2))
    (throw (Exception. (str "operator " operator " supports only 2 arguments"))))
  (let [substitutions {'= '=== '!= '!== 'not= '!==}]
    (str "(" (str/join (str " " (or (substitutions operator) operator) " ")
                       (map #(emit % ctx) args)) ")")))

(def ^{:dynamic true} var-declarations nil)

(defmethod emit-special 'var [type ctx [var & more]]
  (apply swap! var-declarations conj (filter identity (map (fn [name i] (when (odd? i) name)) more (iterate inc 1))))
  (apply str (interleave (map (fn [[name expr]]
                                (str (when-not var-declarations "var ") (emit ctx name) " = " (emit ctx expr)))
                              (partition 2 more))
                         (repeat statement-separator))))

(defn emit-const [more ctx]
  (apply str (interleave (map (fn [[name expr]]
                                (str "const " (emit name ctx) " = " (emit expr ctx)))
                              (partition 2 more))
                         (repeat statement-separator))))


(def ^:dynamic *recur-targets* [])

(defmethod emit-special 'loop* [_ ctx [x bindings & body]]
  (binding [*recur-targets* bindings]
    (format "while (true) {
%s

break; }"
            (emit (list* 'let bindings body) ctx))))

(defmethod emit-special 'recur [_ ctx [_ & exprs]]
  (let [bindings *recur-targets*
        temps (repeatedly (count exprs) gensym)]
    (str
     (str/join "\n"
               (map (fn [temp expr]
                      (statement (format "%s = %s"
                                         temp (emit expr ctx))))
                    temps exprs)
               )
     (str/join "\n"
                (map (fn [binding temp]
                       (statement (format "%s = %s"
                                          binding temp)))
                     bindings temps)
                )
     "continue;\n")))

(defmethod emit-special 'const [_type ctx [_const & more]]
  (emit-const more ctx))

(defmethod emit-special 'def [_type ctx [_const & more]]
  (let [name (first more)]
    (swap! *public-vars* conj (munge* name))
    (emit-const more ctx)))

(declare emit-do)

(defn wrap-await [s]
  (format "(%s)" (str "await " s)))

(defmethod emit-special 'await [_ ctx [_await more]]
  (wrap-await (emit more ctx)))

(defn wrap-iife [s]
  (cond-> (format "(%sfunction () {\n %s\n})()" (if *async* "async " "") s)
    *async* (wrap-await)))

(defn return [s]
  (format "return %s;" s))

(defmethod emit-special 'let* [type ctx [_let bindings & more]]
  (let [partitioned (partition 2 bindings)]
    (wrap-iife
     (str
      (let [names (distinct (map (fn [[name _]]
                                   name)
                                 partitioned))]
        (statement (str "let " (str/join ", " names))))
      (apply str (interleave (map (fn [[name expr]]
                                    (str (emit name ctx) " = " (emit expr ctx)))
                                  partitioned)
                             (repeat statement-separator)))
      (return (emit-do ctx more))))))

(defmethod emit-special 'let [type ctx [_let bindings & more]]
  (emit (core-let bindings more) ctx)
  #_(prn (core-let bindings more)))

(defn process-require-clause [[libname & {:keys [refer as]}]]
  (str (when as
         (statement (format "import * as %s from '%s'" as libname)))
       (when refer
         (statement (format "import { %s } from '%s'"  (str/join ", " refer) libname)))))

(defmethod emit-special 'ns [_type _ctx [_ns _name & clauses]]
  (reduce (fn [acc [k & exprs]]
            (if (= :require k)
              (str acc (str/join "" (map process-require-clause exprs)))
              acc))
          ""
          clauses
          ))

(defmethod emit-special 'funcall [_type ctx [name & args :as expr]]
  (if (and (symbol? name)
           (= "cljs.core" (namespace name)))
    (emit (with-meta (list* (symbol (clojure.core/name name)) args)
            (meta expr)) ctx)
    (str (if (and (list? name) (= 'fn (first name))) ; function literal call
           (str "(" (emit name ctx) ")")
           (let [name
                 (if (contains? core-vars name)
                   (let [name (symbol (munge* name))]
                     (swap! *imported-core-vars* conj name)
                     name)
                   name)]
             (emit name ctx)))
         (comma-list (map #(emit % ctx) args)))))

(defn map-emit [ctx args]
  (map #(emit % ctx) args))

(defmethod emit-special 'str [type ctx [str & args]]
  (apply clojure.core/str (interpose " + " (map-emit ctx args))))

(defn emit-method [ctx obj method args]
  (str (emit obj) "." (emit method) (comma-list (map-emit ctx args))))

(defmethod emit-special '. [type ctx [period obj method & args]]
  (emit-method ctx obj method args))

(defmethod emit-special '.. [type ctx [dotdot & args]]
  (apply str (interpose "." (map-emit ctx args))))

(defmethod emit-special 'if [type ctx [if test true-form & false-form]]
  (str "if (" (emit test ctx) ") { \n"
       (emit true-form ctx)
       "\n }"
       (when (first false-form)
         (str " else { \n"
              (emit (first false-form) ctx)
              " }"))))

(defn emit-aget [ctx var idxs]
  (apply str
         (emit var ctx)
         (interleave (repeat "[") (map-emit ctx idxs) (repeat "]"))))

(defmethod emit-special 'aget [type ctx [_aget var & idxs]]
  (emit-aget ctx var idxs))

(defmethod emit-special 'dot-method [_type ctx [method obj & args]]
  (let [method-str (rstr/drop 1 (str method))]
    (if (str/starts-with? method-str "-")
      (emit-aget ctx obj [(subs method-str 1)])
      (emit-method ctx obj (symbol method-str) args))))

(defmethod emit-special 'return [type ctx [return expr]]
  (statement (str "return " (emit expr ctx))))

#_(defmethod emit-special 'delete [type [return expr]]
  (str "delete " (emit expr)))

(defmethod emit-special 'set! [_type ctx [_set! var val & more]]
  (assert (or (nil? more) (even? (count more))))
  (str (emit var) " = " (emit val ctx) statement-separator
       (when more (str (emit (cons 'set! more) ctx)))))

(defmethod emit-special 'new [_type ctx [_new class & args]]
  (str "new " (emit class ctx) (comma-list (map-emit ctx args))))

(defmethod emit-special 'inc! [_type ctx [_inc var]]
  (str (emit var ctx) "++"))

(defmethod emit-special 'dec! [_type ctx [_dec var]]
  (str (emit var ctx) "--"))

(defmethod emit-special 'dec [_type ctx [_ var]]
  (str "(" (emit var ctx) " - " 1 ")"))

(defmethod emit-special 'inc [_type ctx [_ var]]
  (str "(" (emit var ctx) " + " 1 ")"))

(defmethod emit-special 'defined? [_type ctx [_ var]]
  (str "typeof " (emit var ctx) " !== \"undefined\" && " (emit var ctx) " !== null"))

(defmethod emit-special '? [_type ctx [_ test then else]]
  (str (emit test ctx) " ? " (emit then ctx) " : " (emit else ctx)))

(defmethod emit-special 'and [_type ctx [_ & more]]
  (apply str (interpose "&&" (map-emit ctx more))))

(defmethod emit-special 'or [_type ctx [_ & more]]
  (apply str (interpose "||" (map-emit ctx more))))

(defmethod emit-special 'quote [_type _ctx [_ & more]]
  (apply str more))

(defn emit-do [ctx exprs & [{:keys [top-level?]
                         }]]
  (let [bl (butlast exprs)
        l (last exprs)]
    (cond-> (str (str/join "" (map #(statement (emit % ctx)) bl))
                 (cond-> (emit l ctx)
                   (not top-level?)
                   (return)))
      (not top-level?) (wrap-iife))))

(defmethod emit-special 'do [_type ctx [_ & exprs]]
  (emit-do ctx exprs))

(defmethod emit-special 'while [_type ctx [_while test & body]]
  (str "while (" (emit test) ") { \n"
       (emit-do ctx body)
       "\n }"))

;; TODO: re-implement
(defmethod emit-special 'doseq [_type ctx [_doseq bindings & body]]
  (str "for (" (emit (first bindings) ctx) " in " (emit (second bindings) ctx) ") { \n"
       (if-let [more (nnext bindings)]
         (emit (list* 'doseq more body) ctx)
         (emit-do body ctx))
       "\n }"))

(defn emit-var-declarations []
  #_(when-not (empty? @var-declarations)
      (apply str "var "
             (str/join ", " (map emit @var-declarations))
             statement-separator)))

(declare emit-function*)

(defn emit-function [ctx name sig body & [elide-function?]]
  (assert (or (symbol? name) (nil? name)))
  (assert (vector? sig))
  (let [body (return (emit-do ctx body))]
    (str (when-not elide-function?
           (str (when *async*
                  "async ") "function ")) (comma-list sig) " {\n"
         #_(emit-var-declarations) body "\n}")))

(defn emit-function* [ctx expr]
  (let [name (when (symbol? (first expr)) (first expr))
        expr (if name (rest expr) expr)
        expr (if (seq? (first expr))
               ;; TODO: multi-arity:
               (first expr)
               expr)]
    (if name
      (let [signature (first expr)
            body (rest expr)]
        (str (when *async*
               "async ") "function " name " "
             (emit-function ctx name signature body true)))
      (let [signature (first expr)
            body (rest expr)]
        (str (emit-function ctx nil signature body))))))

(defmethod emit-special 'fn* [_type ctx [_fn & sigs :as expr]]
  (let [async? (:async (meta expr))]
    (binding [*async* async?]
      (emit-function* ctx sigs))))

(defmethod emit-special 'fn [_type ctx [fn & sigs :as expr]]
  (let [expanded (core-fn expr sigs)]
    (emit expanded ctx)))

(defmethod emit-special 'defn [_type ctx [fn name & args :as expr]]
  (let [;;async (:async (meta name))
        [_def _name _fn-expr :as expanded] (core-defn expr {} name args)]
    ;; (prn fn-expr (meta fn-expr))
    (emit expanded ctx)))

(defmethod emit-special 'try [_type ctx [_try & body :as expression]]
  (let [try-body (remove #(contains? #{'catch 'finally} (first %))
                         body)
        catch-clause (filter #(= 'catch (first %))
                             body)
        finally-clause (filter #(= 'finally (first %))
                               body)]
    (cond
      (and (empty? catch-clause)
           (empty? finally-clause))
      (throw (new Exception (str "Must supply a catch or finally clause (or both) in a try statement! " expression)))

      (> (count catch-clause) 1)
      (throw (new Exception (str "Multiple catch clauses in a try statement are not currently supported! " expression)))

      (> (count finally-clause) 1)
      (throw (new Exception (str "Cannot supply more than one finally clause in a try statement! " expression)))

      :true (str "try{\n"
                 (emit-do ctx try-body)
                 "}\n"
                 (if-let [[_ exception & catch-body] (first catch-clause)]
                   (str "catch(" (emit ctx exception) "){\n"
                        (emit-do ctx catch-body)
                        "}\n"))
                 (if-let [[_ & finally-body] (first finally-clause)]
                   (str "finally{\n"
                        (emit-do ctx finally-body)
                        "}\n"))))))

#_(defmethod emit-special 'break [_type _ctx [_break]]
  (statement "break"))

(derive #?(:clj clojure.lang.Cons :cljs Cons) ::list)
(derive #?(:clj clojure.lang.IPersistentList :cljs IList) ::list)
#?(:cljs (derive List ::list))

(defmethod emit ::list [expr ctx]
  (if (symbol? (first expr))
    (let [head (first expr)]
      (cond
        (and (= (rstr/get (str head) 0) \.)
             (> (count (str head)) 1)

             (not (= (rstr/get (str head) 1) \.))) (emit-special 'dot-method ctx expr)
        (contains? built-in-macros head) (let [macro (built-in-macros head)]
                                           (emit (apply macro expr {} (rest expr)) ctx))
        (special-form? head) (emit-special head ctx expr)
        (infix-operator? head) (emit-infix head ctx expr)
        (prefix-unary? head) (emit-prefix-unary head expr)
        (suffix-unary? head) (emit-suffix-unary head expr)
        :else (emit-special 'funcall ctx expr)))
    (if (list? expr)
      (emit-special 'funcall ctx expr)
      (throw (new Exception (str "invalid form: " expr))))))

#?(:cljs (derive PersistentVector ::vector))

(defmethod emit #?(:clj clojure.lang.IPersistentVector
                   :cljs ::vector) [expr ctx]
  (swap! *imported-core-vars* conj 'vector)
  (format "vector(%s)" (str/join ", " (map-emit ctx expr))))

(defmethod emit #?(:clj clojure.lang.LazySeq
                   :cljs LazySeq) [expr ctx]
  (emit (into [] expr) ctx))

#?(:cljs (derive PersistentArrayMap ::map))
#?(:cljs (derive PersistentHashMap ::map))

(defmethod emit #?(:clj clojure.lang.IPersistentMap
                   :cljs ::map) [expr ctx]
  (let [map-fn
        (when-not (::js (meta expr))
          (if (<= (count expr) 8)
            'arrayMap
            'hashMap))
        key-fn (if-not map-fn
                 name identity)
        mk-pair (fn [pair] (str (emit (key-fn (key pair)) ctx) (if map-fn ", " ": ")
                                (emit (val pair) ctx)))
        keys (str/join ", " (map mk-pair (seq expr)))]
    (when map-fn
      (swap! *imported-core-vars* conj map-fn))
    (if map-fn
      (format "%s(%s)" map-fn keys)
      (format "{ %s }" keys))))

(defn transpile-form [f]
  (emit f {}))

(defn transpile-string [s]
  (let [rdr (e/reader s)]
    (loop [transpiled ""]
      (let [next-form (e/parse-next rdr {:readers {'js #(vary-meta % assoc ::js true)}})]
        (if (= ::e/eof next-form)
          transpiled
          (let [next-t (transpile-form next-form)
                next-js (some-> next-t not-empty (statement))]
            (recur (str transpiled next-js))))))))

#?(:cljs
   (defn slurp [f]
     (fs/readFileSync f "utf-8")))

#?(:cljs
   (defn spit [f s]
     (fs/writeFileSync f s "utf-8")))

(defn transpile-file [{:keys [in-file out-file]}]
  (let [core-vars (atom #{})
        public-vars (atom #{})]
    (binding [*imported-core-vars* core-vars
              *public-vars* public-vars]
      (let [out-file (or out-file
                         (str/replace in-file #".cljs$" ".mjs"))
            transpiled (transpile-string (slurp in-file))
            transpiled (if-let [core-vars (seq @core-vars)]
                         (str (format "import { %s } from 'cherry-cljs/cljs.core.js'\n"
                                      (str/join ", " core-vars))
                              transpiled)
                         transpiled)
            transpiled (str transpiled
                            (format "\nexport { %s }\n"
                                    (str/join ", " (disj @public-vars "default$")))
                            (when (contains? @public-vars "default$")
                              "export default default$\n"))]
        (spit out-file transpiled)
        {:out-file out-file}))))
