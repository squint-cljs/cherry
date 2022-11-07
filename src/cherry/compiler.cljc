;; Adapted from Scriptjure. Original copyright notice:

;; Copyright (c) Allen Rohner, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns cherry.compiler
  (:require
   #?(:cljs [goog.string.format])
   #?(:clj [cherry.resource :as resource])
   [cherry.internal.deftype :as deftype]
   [cherry.internal.destructure :refer [core-let]]
   [cherry.internal.fn :refer [core-defmacro core-defn core-fn]]
   [cherry.internal.loop :as loop]
   [cherry.internal.macros :as macros]
   [cherry.internal.protocols :as protocols]
   [clojure.string :as str]
   [edamame.core :as e]
   [squint.compiler-common :as cc :refer [#?(:cljs Exception)
                                          #?(:cljs format)
                                          *aliases* *async* *imported-vars* *public-vars* *recur-targets* *repl* comma-list
                                          emit emit-args emit-do emit-infix emit-let emit-special emit-wrap escape-jsx
                                          expr-env infix-operator? munge* prefix-unary? statement statement-separator
                                          suffix-unary? wrap-iife]])
  #?(:cljs (:require-macros [cherry.resource :as resource])))

(set! cc/infix-operators (disj cc/infix-operators "="))

(defmethod emit #?(:clj clojure.lang.Keyword :cljs Keyword) [expr env]
  (swap! *imported-vars* update "cherry-cljs/lib/cljs_core.js" (fnil conj #{}) 'keyword)
  (emit-wrap (str (format "keyword(%s)" (pr-str (subs (str expr) 1)))) env))

(def special-forms (set ['var '. 'if 'funcall 'fn 'fn* 'quote 'set!
                         'return 'delete 'new 'do 'aget 'while
                         'inc! 'dec! 'dec 'inc 'defined? 'and 'or
                         '? 'try 'break 'throw
                         'js/await 'const 'let 'let* 'ns 'def 'loop*
                         'recur 'js* 'case* 'deftype*
                         ;; prefixed to avoid conflicts
                         'clava-compiler-jsx]))

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
                      'some->> macros/core-some->>
                      'loop loop/core-loop
                      'doseq macros/core-doseq
                      'for macros/core-for
                      'lazy-seq macros/core-lazy-seq
                      'defonce macros/core-defonce
                      'exists? macros/core-exists?
                      'case macros/core-case
                      '.. macros/core-dotdot
                      'defmacro core-defmacro
                      'this-as macros/core-this-as
                      'unchecked-get macros/core-unchecked-get
                      'unchecked-set macros/core-unchecked-set
                      'defprotocol protocols/core-defprotocol
                      'extend-type protocols/core-extend-type
                      'deftype deftype/core-deftype
                      'defn core-defn
                      'defn- core-defn
                      'time macros/core-time
                      'declare macros/core-declare})

(def core-config (resource/edn-resource "cherry/cljs.core.edn"))

(def core-vars (conj (:vars core-config) 'goog_typeOf))

(reset! cc/core-vars core-vars)

(defn special-form? [expr]
  (contains? special-forms expr))

(defn emit-prefix-unary [_type [operator arg]]
  (str operator (emit arg)))

(defn emit-suffix-unary [_type [operator arg]]
  (str (emit arg) operator))

(def ^{:dynamic true} var-declarations nil)

#_(defmethod emit-special 'var [type env [var & more]]
    (apply swap! var-declarations conj (filter identity (map (fn [name i] (when (odd? i) name)) more (iterate inc 1))))
    (apply str (interleave (map (fn [[name expr]]
                                  (str (when-not var-declarations "var ") (emit env name) " = " (emit env expr)))
                                (partition 2 more))
                           (repeat statement-separator))))

(defmethod emit-special 'quote [_ env [_ form]]
  (emit-wrap (emit form (expr-env (assoc env :quote true))) env))

(defmethod emit-special 'deftype* [_ env [_ t fields pmasks body]]
  (let [fields (map munge fields)]
    (str "var " (munge t) " = " (format "function %s {
%s
%s
%s
}"
                                        (comma-list fields)
                                        (str/join "\n"
                                                  (map (fn [fld]
                                                         (str "this." fld " = " fld ";"))
                                                       fields))
                                        (str/join "\n"
                                                  (map (fn [[pno pmask]]
                                                         (str "this.cljs$lang$protocol_mask$partition" pno "$ = " pmask ";"))
                                                       pmasks))
                                        (emit body
                                              (->
                                               env
                                               (update
                                                :var->ident
                                                (fn [vi]
                                                  (merge
                                                   vi
                                                   (zipmap fields
                                                           (map (fn [fld]
                                                                  (symbol (str "self__." fld)))
                                                                fields)))))
                                               (assoc :type true)))))))


#_(defmethod emit* :deftype
    [{:keys [t fields pmasks body protocols]}]
    (let [fields (map munge fields)]
      (emitln "")
      (emitln "/**")
      (emitln "* @constructor")
      (doseq [protocol protocols]
        (emitln " * @implements {" (munge (str protocol)) "}"))
      (emitln "*/")
      (emitln (munge t) " = (function (" (comma-sep fields) "){")
      (doseq [fld fields]
        (emitln "this." fld " = " fld ";"))
      (doseq [[pno pmask] pmasks]
        (emitln "this.cljs$lang$protocol_mask$partition" pno "$ = " pmask ";"))
      (emitln "});")
      (emit body)))

(defmethod emit-special 'case* [_ env [_ v tests thens default]]
  (let [expr? (= :expr (:context env))
        gs (gensym "caseval__")
        eenv (expr-env env)]
    (cond-> (str
             (when expr?
               (str "var " gs ";\n"))
             (str "switch (" (emit v eenv) ") {")
             (str/join (map (fn [test then]
                              (str/join
                               (map (fn [test]
                                      (str (str "case " (emit test eenv) ":\n")
                                           (if expr?
                                             (str gs " = " then)
                                             (emit then env))
                                           "\nbreak;\n"))
                                    test)))
                            tests thens))
             (when default
               (str "default:\n"
                    (if expr?
                      (str gs " = " (emit default eenv))
                      (emit default env))))
             (when expr?
               (str "return " gs ";"))
             "}")
      expr? (wrap-iife))))

(defmethod emit-special 'recur [_ env [_ & exprs]]
  (let [bindings *recur-targets*
        temps (repeatedly (count exprs) gensym)
        eenv (expr-env env)]
    (when-let [cb (:recur-callback env)]
      (cb bindings))
    (str
     (str/join ""
               (map (fn [temp expr]
                      (statement (format "let %s = %s"
                                         temp (emit expr eenv))))
                    temps exprs)
               )
     (str/join ""
               (map (fn [binding temp]
                      (statement (format "%s = %s"
                                         binding temp)))
                    bindings temps)
               )
     "continue;\n")))

(defn emit-var [more env]
  (apply str
         (interleave (map (fn [[name expr]]
                            (str "var " (emit name env) " = "
                                 (emit expr (assoc env :context :expr))))
                          (partition 2 more))
                     (repeat statement-separator))))

(defmethod emit-special 'def [_type env [_const & more]]
  (let [name (first more)]
    (swap! *public-vars* conj (munge* name))
    (emit-var more env)))

(defn wrap-await [s]
  (format "(%s)" (str "await " s)))

(defmethod emit-special 'js/await [_ env [_await more]]
  (emit-wrap (wrap-await (emit more (expr-env env))) env))

(defmethod emit-special 'let [type env [_let bindings & more]]
  (emit (core-let bindings more) env)
  #_(prn (core-let bindings more)))

(defn resolve-ns [alias]
  (case alias
    ;; (squint.string clojure.string) "squint-cljs/string.js"
    alias))

(defn process-require-clause [[libname & {:keys [refer as]}]]
  (let [libname (resolve-ns libname)
        [libname suffix] (str/split libname #"\$" 2)
        [p & _props] (when suffix
                       (str/split suffix #"\."))]
    (str
     (when-not *repl*
       (when (and as (= "default" p))
         (statement (format "import %s from '%s'" as libname))))
     (when (and (not as) (not p) (not refer))
       ;; import presumably for side effects
       (statement (format "import '%s'" libname)))
     (when as
       (swap! *imported-vars* update libname (fnil identity #{}))
       (when *repl*
         (if (str/ends-with? libname "$default")
           (statement (format "import %s from '%s'" as (str/replace libname "$default" "")))
           (statement (format "import * as %s from '%s'"  as libname)))))
     (when refer
       (statement (format "import { %s } from '%s'"  (str/join ", " refer) libname))))))

#_
(defn process-require-clause [[libname & {:keys [refer as]}]]
  (let [[libname suffix] (.split libname "$" 2)
        [p & _props] (when suffix
                       (.split suffix "."))]
    (str
     (when (and as (= "default" p))
       (statement (format "import %s from '%s'" as libname)))
     (when (and as (not p))
       (statement (format "import * as %s from '%s'" as libname)))
     (when refer
       (statement (format "import { %s } from '%s'"  (str/join ", " refer) libname))))))

(defmethod emit-special 'ns [_type _env [_ns name & clauses]]
  (reset! *aliases*
          (->> clauses
               (some
                (fn [[k & exprs]]
                  (when (= :require k) exprs)))
               (reduce
                (fn [aliases [full as alias]]
                  (case as
                    (:as :as-alias)
                    (assoc aliases alias full)
                    aliases))
                {:current name})))
  (reduce (fn [acc [k & exprs]]
            (if (= :require k)
              (str acc (str/join "" (map process-require-clause exprs)))
              acc))
          ""
          clauses
          ))

(defmethod emit-special 'funcall [_type env [fname & args :as _expr]]
  (let [interop? (and (symbol? fname)
                      (= "js" (namespace fname)))]
    (emit-wrap (str
                (emit fname (expr-env env))
                ;; this is needed when calling keywords, symbols, etc. We could
                ;; optimize this later by inferring that we're not directly
                ;; calling a `function`.
                (when-not interop? ".call")
                (comma-list (emit-args env
                                       (if interop? args
                                           (cons nil args)))))
               env
               )))

(defmethod emit-special 'str [_type env [_str & args]]
  (apply clojure.core/str (interpose " + " (emit-args env args))))

(defn emit-method [env obj method args]
  (let [eenv (expr-env env)]
    (emit-wrap (str (emit obj eenv) "."
                    (str method)
                    (comma-list (emit-args env args))) env)))

(defn emit-aget [env var idxs]
  (emit-wrap (apply str
                    (emit var (expr-env env))
                    (interleave (repeat "[") (emit-args env idxs) (repeat "]"))) env))

(defmethod emit-special '. [_type env [_period obj method & args]]
  (let [[method args] (if (seq? method)
                        [(first method) (rest method)]
                        [method args])
        method-str (str method)]
    (if (str/starts-with? method-str "-")
      (emit-aget env obj [(subs method-str 1)])
      (emit-method env obj (symbol method-str) args))) #_(emit-method env obj method args))

(defmethod emit-special 'if [_type env [_if test then else]]
  (swap! *imported-vars* update "cherry-cljs/lib/cljs_core.js" (fnil conj #{}) 'truth_)
  (if (= :expr (:context env))
    (-> (let [env (assoc env :context :expr)]
          (format "(%s) ? (%s) : (%s)"
                  (emit test env)
                  (emit then env)
                  (emit else env)))
        (emit-wrap env))
    (str (format "if (truth_(%s)) {\n"
                 (emit test (assoc env :context :expr)))
         (emit then env)
         "}"
         (when (some? else)
           (str " else {\n"
                (emit else env)
                "}")))))

(defmethod emit-special 'aget [type env [_aget var & idxs]]
  (emit-aget env var idxs))

;; TODO: this should not be reachable in user space
(defmethod emit-special 'return [_type env [_return expr]]
  (statement (str "return " (emit (assoc env :context :expr) env))))

#_(defmethod emit-special 'delete [type [return expr]]
    (str "delete " (emit expr)))

(defmethod emit-special 'set! [_type env [_set! var val & more]]
  (assert (or (nil? more) (even? (count more))))
  (let [eenv (expr-env env)]
    (emit-wrap (str (emit var eenv) " = " (emit val eenv) statement-separator
                    #_(when more (str (emit (cons 'set! more) env)))) env)))

(defmethod emit-special 'new [_type env [_new class & args]]
  (emit-wrap (str "new " (emit class (expr-env env)) (comma-list (emit-args env args))) env))

#_(defmethod emit-special 'inc! [_type env [_inc var]]
    (str (emit var env) "++"))

#_(defmethod emit-special 'dec! [_type env [_dec var]]
    (str (emit var env) "--"))

(defmethod emit-special 'dec [_type env [_ var]]
  (emit-wrap (str "(" (emit var (assoc env :context :expr)) " - " 1 ")") env))

(defmethod emit-special 'inc [_type env [_ var]]
  (emit-wrap (str "(" (emit var (assoc env :context :expr)) " + " 1 ")") env))

#_(defmethod emit-special 'defined? [_type env [_ var]]
    (str "typeof " (emit var env) " !== \"undefined\" && " (emit var env) " !== null"))

#_(defmethod emit-special '? [_type env [_ test then else]]
    (str (emit test env) " ? " (emit then env) " : " (emit else env)))

(defmethod emit-special 'and [_type env [_ & more]]
  (emit-wrap (apply str (interpose " && " (emit-args env more))) env))

(defmethod emit-special 'or [_type env [_ & more]]
  (emit-wrap (apply str (interpose " || " (emit-args env more))) env))


(defmethod emit-special 'while [_type env [_while test & body]]
  (str "while (" (emit test) ") { \n"
       (emit-do env body)
       "\n }"))

;; TODO: re-implement
#_(defmethod emit-special 'doseq [_type env [_doseq bindings & body]]
    (str "for (" (emit (first bindings) env) " in " (emit (second bindings) env) ") { \n"
         (if-let [more (nnext bindings)]
           (emit (list* 'doseq more body) env)
           (emit-do body env))
         "\n }"))

(defn emit-var-declarations []
  #_(when-not (empty? @var-declarations)
      (apply str "var "
             (str/join ", " (map emit @var-declarations))
             statement-separator)))

(declare emit-function*)

(defn ->sig [env sig]
  (reduce (fn [[env sig seen] param]
            (if (contains? seen param)
              (let [new-param (gensym param)
                    env (update env :var->ident assoc param new-param)
                    sig (conj sig new-param)
                    seen (conj seen param)]
                [env sig seen])
              [env (conj sig param) (conj seen param)]))
          [env [] #{}]
          sig))

(defn emit-function [env name sig body & [elide-function?]]
  (assert (or (symbol? name) (nil? name)))
  (assert (vector? sig))
  (let [[env sig] (->sig env sig)]
    (binding [*recur-targets* sig]
      (let [recur? (volatile! nil)
            env (assoc env :recur-callback
                       (fn [coll]
                         (when (identical? sig coll)
                           (vreset! recur? true))))
            body (emit-do (assoc env :context :return) body)
            body (if @recur?
                   (format "while(true){
%s
break;}" body)
                   body)]
        (str (when-not elide-function?
               (str (when *async*
                      "async ") "function "))
             (comma-list (map munge sig)) " {\n"
             (when (:type env)
               (str "var self__ = this;"))
             body "\n}")))))

#_(defn emit-function [env name sig body & [elide-function?]]
    (assert (or (symbol? name) (nil? name)))
    (assert (vector? sig))
    (binding [*recur-targets* sig]
      (let [recur? (volatile! nil)
            env (assoc env :recur-callback
                       (fn [coll]
                         (when (identical? sig coll)
                           (vreset! recur? true))))
            body (emit-do (assoc env :context :return) body)
            body (if @recur?
                   (format "while(true){
%s
break;}" body)
                   body)]
        (str (when-not elide-function?
               (str (when *async*
                      "async ") "function "))
             (comma-list (map munge sig)) " {\n"
             (when (:type env)
               (str "var self__ = this;"))
             body "\n}"))))

(defn emit-function* [env expr]
  (let [name (when (symbol? (first expr)) (first expr))
        expr (if name (rest expr) expr)
        expr (if (seq? (first expr))
               ;; TODO: multi-arity:
               (first expr)
               expr)]
    (-> (if name
          (let [signature (first expr)
                body (rest expr)]
            (str (when *async*
                   "async ") "function " name " "
                 (emit-function env name signature body true)))
          (let [signature (first expr)
                body (rest expr)]
            (str (emit-function env nil signature body))))
        (emit-wrap env))))

(defmethod emit-special 'fn* [_type env [_fn & sigs :as expr]]
  (let [async? (:async (meta expr))]
    (binding [*async* async?]
      (emit-function* env sigs))))

(defmethod emit-special 'fn [_type env [_fn & sigs :as expr]]
  (let [expanded (apply core-fn expr {} sigs)]
    (emit expanded env)))

(defmethod emit-special 'try [_type env [_try & body :as expression]]
  (let [try-body (remove #(contains? #{'catch 'finally} (and (seq? %)
                                                             (first %)))
                         body)
        catch-clause (filter #(= 'catch (and (seq? %)
                                             (first %)))
                             body)
        finally-clause (filter #(= 'finally (and (seq? %)
                                                 (first %)))
                               body)]
    (cond
      (and (empty? catch-clause)
           (empty? finally-clause))
      (throw (new Exception (str "Must supply a catch or finally clause (or both) in a try statement! " expression)))

      (> (count catch-clause) 1)
      (throw (new Exception (str "Multiple catch clauses in a try statement are not currently supported! " expression)))

      (> (count finally-clause) 1)
      (throw (new Exception (str "Cannot supply more than one finally clause in a try statement! " expression)))

      :else
      (-> (cond-> (str "try{\n"
                       (emit-do env try-body)
                       "}\n"
                       (when-let [[_ _exception binding & catch-body] (first catch-clause)]
                         ;; TODO: only bind when exception type matches
                         (str "catch(" (emit binding (expr-env env)) "){\n"
                              (emit-do env catch-body)
                              "}\n"))
                       (when-let [[_ & finally-body] (first finally-clause)]
                         (str "finally{\n"
                              (emit-do (assoc env :context :statement) finally-body)
                              "}\n")))
            (not= :statement (:context env))
            (wrap-iife))
          (emit-wrap env)))))

#_(defmethod emit-special 'break [_type _env [_break]]
    (statement "break"))

(derive #?(:clj clojure.lang.Cons :cljs Cons) ::list)
(derive #?(:clj clojure.lang.IPersistentList :cljs IList) ::list)
(derive #?(:clj clojure.lang.LazySeq :cljs LazySeq) ::list)
#?(:cljs (derive List ::list))

(defn strip-core-symbol [sym]
  (let [sym-ns (namespace sym)]
    (if (and sym-ns
             (or (= "clojure.core" sym-ns)
                 (= "cljs.core" sym-ns)))
      (symbol (name sym))
      sym)))

(defmethod emit ::list [expr env]
  (escape-jsx
   (let [env (dissoc env :jsx)]
     (if (:quote env)
       (do
         (swap! *imported-vars* update "cherry-cljs/lib/cljs_core.js" (fnil conj #{}) 'list)
         (format "list(%s)"
                 (str/join ", " (emit-args env expr))))
       (cond (symbol? (first expr))
             (let [head* (first expr)
                   head (strip-core-symbol head*)
                   expr (if (not= head head*)
                          (with-meta (cons head (rest expr))
                            (meta expr))
                          expr)
                   head-str (str head)]
               (cond
                 (and (= (.charAt head-str 0) \.)
                      (> (count head-str) 1)
                      (not (= ".." head-str)))
                 (emit-special '. env
                               (list* '.
                                      (second expr)
                                      (symbol (subs head-str 1))
                                      (nnext expr)))
                 (contains? built-in-macros head)
                 (let [macro (built-in-macros head)
                       new-expr (apply macro expr {} (rest expr))]
                   (emit new-expr env))
                 (and (> (count head-str) 1)
                      (str/ends-with? head-str "."))
                 (emit (list* 'new (symbol (subs head-str 0 (dec (count head-str)))) (rest expr))
                       env)
                 (special-form? head) (emit-special head env expr)
                 (infix-operator? head) (emit-infix head env expr)
                 (prefix-unary? head) (emit-prefix-unary head expr)
                 (suffix-unary? head) (emit-suffix-unary head expr)
                 :else (emit-special 'funcall env expr)))
             (list? expr)
             (emit-special 'funcall env expr)
             :else
             (throw (new Exception (str "invalid form: " expr))))))
   env))

#?(:cljs (derive PersistentVector ::vector))

#_(defn wrap-expr [env s]
    (case (:context env)
      :expr (wrap-iife s)
      :statement s
      :return s))

(defn jsx-attrs [v env]
  (let [env (expr-env env)]
    (if v
      (str " "
           (str/join " "
                     (map (fn [[k v]]
                            (str (name k) "=" (emit v (assoc env :jsx-attr true))))
                          v)))
      "")))

(defmethod emit #?(:clj clojure.lang.IPersistentVector
                   :cljs ::vector) [expr env]
  (if (and (:jsx env)
           (let [f (first expr)]
             (or (keyword? f)
                 (symbol? f))))
    (let [v expr
          tag (first v)
          attrs (second v)
          attrs (when (map? attrs) attrs)
          elts (if attrs (nnext v) (next v))
          tag-name (symbol tag)
          tag-name (if (= '<> tag-name)
                     (symbol "")
                     tag-name)]
      (emit-wrap (format "<%s%s>%s</%s>"
                         tag-name
                         (jsx-attrs attrs env)
                         (let [env (expr-env env)]
                           (str/join " " (map #(emit % env) elts)))
                         tag-name) env))
    (if (::js (meta expr))
      (emit-wrap (format "[%s]"
                         (str/join ", " (emit-args env expr))) env)
      (do (swap! *imported-vars* update "cherry-cljs/lib/cljs_core.js" (fnil conj #{}) 'vector)
          (emit-wrap (format "vector(%s)"
                             (str/join ", " (emit-args env expr))) env)))))

#?(:cljs (derive PersistentArrayMap ::map))
#?(:cljs (derive PersistentHashMap ::map))

(defmethod emit #?(:clj clojure.lang.IPersistentMap
                   :cljs ::map) [expr env]
  (let [env* env
        env (dissoc env :jsx)
        expr-env (assoc env :context :expr)
        map-fn
        (when-not (::js (meta expr))
          (if (<= (count expr) 8)
            'arrayMap
            'hashMap))
        key-fn (if-not map-fn
                 name identity)
        mk-pair (fn [pair] (str (emit (key-fn (key pair)) expr-env) (if map-fn ", " ": ")
                                (emit (val pair) expr-env)))
        keys (str/join ", " (map mk-pair (seq expr)))]
    (when map-fn
      (swap! *imported-vars* update "cherry-cljs/lib/cljs_core.js" (fnil conj #{}) map-fn))
    (escape-jsx (-> (if map-fn
                      (format "%s(%s)" map-fn keys)
                      (format "({ %s })" keys))
                    (emit-wrap env)) env*)))

(defmethod emit #?(:clj clojure.lang.PersistentHashSet
                   :cljs PersistentHashSet)
  [expr env]
  (swap! *imported-vars* update "cherry-cljs/lib/cljs_core.js" (fnil conj #{}) 'hash_set)
  (emit-wrap (format "%s%s" "hash_set"
                     (comma-list (emit-args env expr))) env))

(defn transpile-form [f]
  (emit f {:context :statement}))

(def ^:dynamic *jsx* false)

(defn jsx [form]
  (list 'clava-compiler-jsx form))

(defmethod emit-special 'clava-compiler-jsx [_ env [_ form]]
  (set! *jsx* true)
  (let [env (assoc env :jsx true)]
    (emit form env)))

(def cherry-parse-opts
  (e/normalize-opts
   {:all true
    :end-location false
    :location? seq?
    :readers {'js #(vary-meta % assoc ::js true)
              'jsx jsx}
    :read-cond :allow
    :features #{:cljc}}))

(defn transpile-string* [s]
  (let [rdr (e/reader s)
        opts cherry-parse-opts]
    (loop [transpiled ""]
      (let [opts (assoc opts :auto-resolve @*aliases*)
            next-form (e/parse-next rdr opts)]
        (if (= ::e/eof next-form)
          transpiled
          (let [next-t (transpile-form next-form)
                next-js (some-> next-t not-empty (statement))]
            (recur (str transpiled next-js))))))))

(defn compile-string*
  ([s] (compile-string* s nil))
  ([s {:keys [elide-exports
              elide-imports]}]
   (let [imported-vars (atom {})
         public-vars (atom #{})
         aliases (atom {})]
     (binding [*imported-vars* imported-vars
               *public-vars* public-vars
               *aliases* aliases
               *jsx* false
               cc/*core-package* "cherry-cljs/lib/cljs_core.js"]
       (let [transpiled (transpile-string* s)
             imports (when-not elide-imports
                       (let [ns->alias (zipmap (vals @aliases)
                                               (keys @aliases))]
                         (reduce (fn [acc [k v]]
                                   (let [alias (get ns->alias k)
                                         symbols (if alias
                                                   (map #(str % " as " (str alias "_" %)) v)
                                                   v)]
                                     (str acc
                                          (when (or (not *repl*)
                                                    (seq symbols))
                                            (format "import { %s } from '%s'\n"
                                                    (str/join ", " symbols)
                                                    k)))))
                                 ""
                                 @imported-vars)))
             #_#_imports (when-let [core-vars (and (not elide-imports)
                                               (seq @imported-vars))]
                       (str (format "import { %s } from 'cherry-cljs/cljs.core.js'\n"
                                    (str/join ", " core-vars))))
             exports (when-not elide-exports
                       (str (when-let [vars (disj @public-vars "default$")]
                              (when (seq vars)
                                (str (format "\nexport { %s }\n"
                                             (str/join ", " vars))
                                     )))
                            (when (contains? @public-vars "default$")
                              "export default default$\n")))]
         {:imports imports
          :exports exports
          :body transpiled
          :javascript (str imports transpiled exports)
          :jsx *jsx*})))))

(defn compile-string
  ([s] (compile-string s nil))
  ([s opts]
   (let [{:keys [imports exports body]}
         (compile-string* s opts)]
     (str imports body exports))))

#_(defn compile! [s]
    (prn :s s)
    (let [expr (e/parse-string s {:row-key :line
                                  :col-key :column
                                  :end-location false})
          compiler-env (ana-api/empty-state)]
      (prn :expr expr (meta expr))
      (binding [cljs.env/*compiler* compiler-env
                ana/*cljs-ns* 'cljs.user]
        (let [analyzed (ana/analyze (ana/empty-env) expr)]
          (prn (keys analyzed))
          (prn (compiler/emit-str analyzed))))))
