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
   #?(:clj [cherry.resource :as resource])
   #?(:cljs [goog.string.format])
   [cherry.internal.deftype :as deftype]
   [cherry.internal.destructure :refer [core-let]]
   [cherry.internal.fn :refer [core-defmacro core-defn core-fn]]
   [cherry.internal.loop :as loop]
   [cherry.internal.macros :as macros]
   [cherry.internal.protocols :as protocols]
   [squint.internal.macros :as squint-macros]
   [clojure.string :as str]
   [edamame.core :as e]
   [squint.compiler-common :as cc :refer [#?(:cljs Exception)
                                          #?(:cljs format)
                                          *aliases* *imported-vars* *public-vars* comma-list emit emit-args emit-infix
                                          emit-return escape-jsx expr-env infix-operator? prefix-unary?

                                          statement suffix-unary?]]
   [squint.defclass :as defclass])
  #?(:cljs (:require-macros [cherry.resource :as resource])))

#?(:clj (defmacro set-var! [the-var value]
          `(alter-var-root (var ~the-var) (constantly ~value))))

(defn emit-keyword [expr env]
  (swap! *imported-vars* update "cherry-cljs/lib/cljs.core.js" (fnil conj #{}) 'keyword)
  (emit-return (str (format "%skeyword(%s)"
                            (if-let [core-alias (:core-alias env)]
                              (str core-alias ".")
                              "")
                            (pr-str (subs (str expr) 1)))) env))

(def special-forms (set ['var '. 'if 'funcall 'fn 'fn* 'quote 'set!
                         'return 'delete 'new 'do 'aget 'while
                         'inc! 'dec! 'dec 'inc 'defined? 'and 'or
                         '? 'try 'break 'throw
                         'js/await 'js-await 'const 'let 'let* 'letfn* 'ns 'require 'def 'loop*
                         'recur 'js* 'case* 'deftype*
                         ;; prefixed to avoid conflicts
                         'clava-compiler-jsx
                         'squint.defclass/defclass* 'squint.defclass/super*]))

(def built-in-macros (merge {'-> macros/core->
                             '->> macros/core->>
                             'as-> macros/core-as->
                             'comment macros/core-comment
                             'dotimes squint-macros/core-dotimes
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
                             'declare macros/core-declare
                             'letfn macros/core-letfn
                             'with-out-str macros/core-with-out-str
                             'binding macros/core-binding
                             'with-redefs macros/core-with-redefs
                             'defclass defclass/defclass
                             'js-template defclass/js-template
                             'and squint-macros/core-and
                             'or squint-macros/core-or
                             }
                            cc/common-macros))

(def core-config (resource/edn-resource "cherry/cljs.core.edn"))

(def core-vars (conj (:vars core-config) 'goog_typeOf))

(defmulti emit-special (fn [disp _env & _args] disp))

(defn special-form? [expr]
  (or
   (contains? cc/special-forms expr)
   (contains? special-forms expr)))

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

(defmethod emit-special 'squint.defclass/defclass* [_ env form]
  (defclass/emit-class env emit form))

(defmethod emit-special 'squint.defclass/super* [_ env form]
  (defclass/emit-super env emit (second form)))

(defmethod emit-special 'fn [_type env [_fn & sigs :as expr]]
  ;; (prn :expr expr)
  (let [expanded (apply core-fn expr {} sigs)]
    ;; (prn :expanded expanded)
    (emit expanded env)))

#_(defmethod emit-special 'break [_type _env [_break]]
    (statement "break"))

(defn strip-core-symbol [sym]
  (let [sym-ns (namespace sym)]
    (if (and sym-ns
             (or (= "clojure.core" sym-ns)
                 (= "cljs.core" sym-ns)))
      (symbol (name sym))
      sym)))

(defn emit-list [expr env]
  (escape-jsx
   (let [env (dissoc env :jsx)]
     (if (:quote env)
       (do
         (swap! *imported-vars* update "cherry-cljs/lib/cljs.core.js" (fnil conj #{}) 'list)
         (format "%slist(%s)"
                 (if-let [ca (:core-alias env)]
                   (str ca ".")
                   "")
                 (str/join ", " (emit-args env expr))))
       (cond (symbol? (first expr))
             (let [head* (first expr)
                   head (strip-core-symbol head*)
                   expr (if (not= head head*)
                          (with-meta (cons head (rest expr))
                            (meta expr))
                          expr)
                   head-str (str head)
                   macro (when (symbol? head)
                           (or (get built-in-macros head)
                               (let [ns (namespace head)
                                     nm (name head)]
                                 (when (and ns nm)
                                   (some-> env :macros (get (symbol ns)) (get (symbol nm)))))))]
               (if macro
                 (let [;; fix for calling macro with more than 20 args
                       #?@(:cljs [macro (or (.-afn ^js macro) macro)])
                       new-expr (apply macro expr {} (rest expr))]
                   (emit new-expr env))
                 (cond
                   (and (= (.charAt head-str 0) \.)
                        (> (count head-str) 1)
                        (not (= ".." head-str)))
                   (cc/emit-special '. env
                                    (list* '.
                                           (second expr)
                                           (symbol (subs head-str 1))
                                           (nnext expr)))
                   (and (> (count head-str) 1)
                        (str/ends-with? head-str "."))
                   (emit (list* 'new (symbol (subs head-str 0 (dec (count head-str)))) (rest expr))
                         env)
                   (special-form? head) (cc/emit-special head env expr)
                   (infix-operator? env head) (emit-infix head env expr)
                   (prefix-unary? head) (emit-prefix-unary head expr)
                   (suffix-unary? head) (emit-suffix-unary head expr)
                   :else (cc/emit-special 'funcall env expr))))
             (list? expr)
             (cc/emit-special 'funcall env expr)
             :else
             (throw (new Exception (str "invalid form: " expr))))))
   env))

#_(defn wrap-expr [env s]
    (case (:context env)
      :expr (wrap-iife s)
      :statement s
      :return s))

(defn emit-vector [expr env]
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
                           (cc/jsx-attrs attrs env)
                           (let [env (expr-env env)]
                             (str/join "" (map #(emit % env) elts)))
                           tag-name) env))
    (if (::js (meta expr))
      (emit-return (format "[%s]"
                           (str/join ", " (emit-args env expr))) env)
      (emit-return (format "%svector(%s)"
                           (if-let [core-alias (:core-alias env)]
                             (str core-alias ".")
                             "")
                           (str/join ", " (emit-args env expr))) env))))

(defn emit-map [expr env]
  (let [env* env
        env (dissoc env :jsx)
        expr-env (assoc env :context :expr)
        map-fn
        (when-not (::js (meta expr))
          (if (<= (count expr) 8)
            'array_map
            'hash_map))
        key-fn (if-not map-fn
                 name identity)
        mk-pair (fn [pair] (str (emit (key-fn (key pair)) expr-env) (if map-fn ", " ": ")
                                (emit (val pair) expr-env)))
        keys (str/join ", " (map mk-pair (seq expr)))]
    (when map-fn
      (swap! *imported-vars* update "cherry-cljs/lib/cljs.core.js" (fnil conj #{}) map-fn))
    (escape-jsx (-> (if map-fn
                      (format "%s%s(%s)"
                              (if-let [ca (:core-alias env)]
                                (str ca ".")
                                "")
                              map-fn keys)
                      (format "({ %s })" keys))
                    (emit-return env)) env*)))

(defn emit-set [expr env]
  (swap! *imported-vars* update "cherry-cljs/lib/cljs.core.js" (fnil conj #{}) 'hash_set)
  (emit-return (format "%s%s" (format "%shash_set"
                                      (if-let [ca (:core-alias env)]
                                        (str ca ".")
                                        ""))
                       (comma-list (emit-args env expr))) env))

(defn transpile-form-internal
  ([f] (transpile-form-internal f nil))
  ([f opts]
   (binding [cc/*target* :cherry
             cc/*repl* (:repl opts cc/*repl*)]
     (str
      (emit f (merge {:ns-state (atom {})
                      :context :statement
                      :core-vars core-vars
                      :infix-operators (disj cc/infix-operators "=")
                      :gensym (let [ctr (volatile! 0)]
                                (fn [sym]
                                  (let [next-id (vswap! ctr inc)]
                                    (symbol (str (if sym (munge sym)
                                                     "G__") next-id)))))
                      :emit {::cc/list emit-list
                             ::cc/vector emit-vector
                             ::cc/map emit-map
                             ::cc/keyword emit-keyword
                             ::cc/set emit-set
                             ::cc/special emit-special}} opts))))))

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
    :features #{:cljs :cherry}}))

(defn transpile-string-internal [s compiler-opts]
  (let [rdr (e/reader s)
        opts cherry-parse-opts
        opts (merge {:ns-state (atom {})}
                    opts)]
    (loop [transpiled (if cc/*repl*
                        (str "globalThis." cc/*cljs-ns* " = globalThis." cc/*cljs-ns* " || {};\n")
                        "")]
      (let [opts (assoc opts :auto-resolve @*aliases*)
            next-form (e/parse-next rdr opts)]
        (if (= ::e/eof next-form)
          transpiled
          (let [next-t (transpile-form-internal next-form compiler-opts)
                next-js (some-> next-t not-empty (statement))]
            (recur (str transpiled next-js))))))))

(defn compile-internal
  ([x f {:keys [elide-exports
                elide-imports
                core-alias
                aliases]
         :or {core-alias "cherry_core"}
         :as opts}]
   (let [opts (merge {:ns-state (atom {})} opts)]
     (binding [cc/*core-package* "cherry-cljs/cljs.core.js"
               *jsx* false
               cc/*repl* (:repl opts cc/*repl*)]
       (let [opts (merge {:ns-state (atom {})} opts)
             imported-vars (atom {})
             public-vars (atom #{})
             aliases (atom (merge aliases {core-alias cc/*core-package*}))
             imports (atom (if cc/*repl*
                             (format "var %s = await import('%s');\n"
                                     core-alias cc/*core-package*)
                             (format "import * as %s from '%s';\n"
                                     core-alias cc/*core-package*)))]
         (binding [*imported-vars* imported-vars
                   *public-vars* public-vars
                   *aliases* aliases
                   cc/*target* :squint
                   cc/*async* (:async opts)
                   cc/*cljs-ns* (:ns opts cc/*cljs-ns*)]
           (let [transpiled (f x (assoc opts
                                        :core-alias core-alias
                                        :imports imports))
                 imports (when-not elide-imports @imports)
                 exports (when-not elide-exports
                           (str (when-let [vars (disj @public-vars "default$")]
                                  (when (seq vars)
                                    (str (format "\nexport { %s }\n"
                                                 (str/join ", " vars)))))
                                (when (contains? @public-vars "default$")
                                  "export default default$\n")))]
             {:imports imports
              :exports exports
              :body transpiled
              :javascript (str imports transpiled exports)
              :jsx *jsx*
              :ns cc/*cljs-ns*
              :ns-state (:ns-state opts)})))))))

(defn compile-string*
  ([s] (compile-string* s nil))
  ([s opts] (compile-string* s opts nil))
  ([s opts state]
   (compile-internal s transpile-string-internal (merge state opts))))

#?(:cljs
   (defn clj-ize-opts [opts]
     (cond-> opts
       (:context opts) (update :context keyword)
       (:ns opts) (update :ns symbol)
       (:elide_imports opts) (assoc :elide-imports (:elide_imports opts))
       (:elide_exports opts) (assoc :elide-exports (:elide_exports opts)))))

#?(:cljs
   (defn compileStringEx [s opts state]
     (let [opts (js->clj opts :keywordize-keys true)
           state (js->clj state :keywordize-keys true)]
       (clj->js (compile-string* s (clj-ize-opts opts) (clj-ize-opts state))))))

(defn compile-string
  ([s] (compile-string s nil))
  ([s opts]
   (let [opts #?(:cljs (if (object? opts)
                         (cond-> (js->clj opts :keywordize-keys true)
                           :context (update :context keyword))
                         opts)
                 :default opts)
         {:keys [imports exports body]}
         (compile-string* s opts)]
     (str imports body exports))))

(defn compile-form*
  ([form] (compile-form* form nil))
  ([s opts]
   (compile-internal s transpile-form-internal opts)))

(defn compile-form
  ([form] (compile-form form nil))
  ([s opts]
   (let [{:keys [imports exports body]}
         (compile-form* s opts)]
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
