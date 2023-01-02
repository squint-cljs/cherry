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
                                          *aliases* *imported-vars* *public-vars* *repl* comma-list emit emit-args emit-infix
                                          emit-special emit-return escape-jsx expr-env infix-operator? prefix-unary?
                                          statement suffix-unary?]])
  #?(:cljs (:require-macros [cherry.resource :as resource])))

#?(:clj (defmacro set-var! [the-var value]
          `(alter-var-root (var ~the-var) (constantly ~value))))

(#?(:clj set-var!
    :cljs set!) cc/infix-operators (disj cc/infix-operators "="))

(defmethod emit #?(:clj clojure.lang.Keyword :cljs Keyword) [expr env]
  (swap! *imported-vars* update "cherry-cljs/lib/cljs_core.js" (fnil conj #{}) 'keyword)
  (emit-return (str (format "keyword(%s)" (pr-str (subs (str expr) 1)))) env))

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
  (emit-return (emit form (expr-env (assoc env :quote true))) env))

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

(defmethod emit-special 'let [_type env [_let bindings & more]]
  (emit (core-let bindings more) env)
  #_(prn (core-let bindings more)))

(defmethod emit-special 'if [_type env [_if test then else]]
  (swap! *imported-vars* update "cherry-cljs/lib/cljs_core.js" (fnil conj #{}) 'truth_)
  (if (= :expr (:context env))
    (-> (let [env (assoc env :context :expr)]
          (format "(%s) ? (%s) : (%s)"
                  (emit test env)
                  (emit then env)
                  (emit else env)))
        (emit-return env))
    (str (format "if (truth_(%s)) {\n"
                 (emit test (assoc env :context :expr)))
         (emit then env)
         "}"
         (when (some? else)
           (str " else {\n"
                (emit else env)
                "}")))))

(defmethod emit-special 'fn [_type env [_fn & sigs :as expr]]
  ;; (prn :expr expr)
  (let [expanded (apply core-fn expr {} sigs)]
    ;; (prn :expanded expanded)
    (emit expanded env)))

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
                       ;; fix for calling macro with more than 20 args
                       #?@(:cljs [macro (or (.-afn ^js macro) macro)])
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
                     tag-name)
          tag-name (emit tag-name (expr-env (dissoc env :jsx)))]
      (emit-return (format "<%s%s>%s</%s>"
                         tag-name
                         (jsx-attrs attrs env)
                         (let [env (expr-env env)]
                           (str/join " " (map #(emit % env) elts)))
                         tag-name) env))
    (if (::js (meta expr))
      (emit-return (format "[%s]"
                         (str/join ", " (emit-args env expr))) env)
      (do (swap! *imported-vars* update "cherry-cljs/lib/cljs_core.js" (fnil conj #{}) 'vector)
          (emit-return (format "vector(%s)"
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
                    (emit-return env)) env*)))

(defmethod emit #?(:clj clojure.lang.PersistentHashSet
                   :cljs PersistentHashSet)
  [expr env]
  (swap! *imported-vars* update "cherry-cljs/lib/cljs_core.js" (fnil conj #{}) 'hash_set)
  (emit-return (format "%s%s" "hash_set"
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
               cc/*core-package* "cherry-cljs/lib/cljs_core.js"
               cc/*target* :cherry]
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
