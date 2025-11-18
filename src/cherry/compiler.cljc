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
                                          emit-return escape-jsx infix-operator? prefix-unary?

                                          statement suffix-unary?]]
   [squint.defclass :as defclass])
  #?(:cljs (:require-macros [cherry.resource :as resource])))

#?(:clj (defmacro set-var! [the-var value]
          `(alter-var-root (var ~the-var) (constantly ~value))))

(defn emit-keyword [expr env]
  (let [js? (:js env)] ;; js is used for emitting CSS literals
    (if (or (:html-attr env)
            js?)
      (cond-> (name expr)
        js? (pr-str))
      (do
        (swap! *imported-vars* update "cherry-cljs/lib/cljs.core.js" (fnil conj #{}) 'keyword)
        (-> (emit-return (format "%skeyword(%s)"
                                 (if-let [core-alias (:core-alias env)]
                                   (str core-alias ".")
                                   "")
                                 (pr-str (subs (str expr) 1))) env)
            (escape-jsx env))))))

(def special-forms (set ['var '. 'if 'funcall 'fn 'fn* 'quote 'set!
                         'return 'delete 'new 'do 'aget 'aset 'while
                         'inc! 'dec! 'dec 'inc 'defined? 'and 'or
                         '? 'try 'break 'throw
                         'js/await 'js-await 'const 'let 'let* 'letfn* 'ns 'require 'def 'loop*
                         'recur 'js* 'case* 'deftype*
                         ;; prefixed to avoid conflicts
                         'clava-compiler-jsx
                         'squint.defclass/defclass* 'squint.defclass/super*
                         'squint-compiler-html]))

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
                             'assert squint-macros/core-assert
                             'satisfies? macros/core-satisfies?
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
                 (= "cljs.core" sym-ns)
                 (= "cherry.core" sym-ns)))
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
                   mexpr (meta expr)
                   expr (if (not= head head*)
                          (with-meta (cons head (rest expr))
                            mexpr)
                          expr)
                   head-str (str head)
                   macro (when (and (symbol? head)
                                    (not (:squint.compiler/skip-macro mexpr)))
                           (or (built-in-macros (strip-core-symbol head))
                               (let [ns (namespace head)
                                     nm (name head)
                                     ns-state @(:ns-state env)
                                     current-ns (:current ns-state)
                                     nms (symbol nm)
                                     current-ns-state (get ns-state current-ns)]
                                 (if ns
                                   (let [nss (symbol ns)]
                                     (or
                                      ;; used by cherry embed:
                                      (some-> env :macros (get nss) (get nms))
                                      (let [resolved-ns (get-in current-ns-state [:aliases nss] nss)]
                                        (get-in ns-state [:macros resolved-ns nms]))))
                                   (let [refers (:refers current-ns-state)]
                                     (when-let [macro-ns (get refers nms)]
                                       (or (some-> env :macros (get (symbol macro-ns)) (get nms))
                                           (get-in ns-state [:macros macro-ns nms]))))))))]
               (if macro
                 (let [;; fix for calling macro with more than 20 args
                       #?@(:cljs [macro (or (.-afn ^js macro) macro)])
                       new-expr (apply macro expr (assoc env
                                                         ;; Added for CLJS compat
                                                         :ns {:name cc/*cljs-ns*}
                                                         :utils {:emit emit})
                                       (rest expr))]
                   (emit new-expr env))
                 (cond
                   (and (= \. (.charAt head-str 0))
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

(defn emit-map [expr env]
  (let [env* env
        env (dissoc env :jsx)
        expr-env (assoc env :context :expr)
        map-fn
        (when-not (::cc/js (meta expr))
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
                             ::cc/vector cc/emit-vector
                             ::cc/map emit-map
                             ::cc/keyword emit-keyword
                             ::cc/set emit-set
                             ::cc/special emit-special}} opts))))))

(def ^:dynamic *jsx* false)

(defn jsx [form]
  (list 'clava-compiler-jsx form))

(defn html [form]
  (list 'squint-compiler-html form))

(defmethod emit-special 'clava-compiler-jsx [_ env [_ form]]
  (set! *jsx* true)
  (let [env (assoc env :jsx true)]
    (emit form env)))

(def cherry-parse-opts
  (e/normalize-opts
   {:all true
    :end-location false
    :location? seq?
    :readers {'js #(vary-meta % assoc ::cc/js true)
              'jsx jsx
              'html html}
    :read-cond :allow
    :features #{:cljs :cherry}}))

(defn read-forms [s]
  (e/parse-string-all s (assoc cherry-parse-opts
                               :auto-resolve-ns true
                               :auto-resolve @*aliases*)))

(defn transpile-internal [s env]
  (let [env (merge {:ns-state (atom {})
                    :context :statement} env)
        forms (if (string? s)
                (read-forms s)
                [s])
        max-form-idx (dec (count forms))
        return? (= :return (:context env))
        env (if return? (assoc env :context :statement) env)]
    (loop [transpiled (if cc/*repl*
                        (let [ns (munge cc/*cljs-ns*)]
                          (cc/ensure-global ns))
                        "")
           forms forms
           form-idx 0]
      (let [next-form (if (seq forms)
                        (first forms) ::e/eof)
            last? (= form-idx max-form-idx)
            env (if (and return? last?)
                  (assoc env :context :return)
                  env)]
        (if (= ::e/eof next-form)
          transpiled
          (let [next-t (-> (transpile-form-internal next-form env)
                           not-empty)
                next-js
                (cc/save-pragma env next-t)]
            (recur (str transpiled next-js)
                   (rest forms)
                   (inc form-idx))))))))

(defn compile-internal
  ([x {:keys [elide-exports
              elide-imports
              core-alias]
       :or {core-alias "cherry_core"}
       :as opts}]
   (let [opts (merge {:ns-state (atom {})} opts)]
     (binding [cc/*core-package* "cherry-cljs/cljs.core.js"
               *jsx* false
               cc/*repl* (:repl opts cc/*repl*)]
       (let [need-html-import (atom false)
             opts (merge {:ns-state (atom {})
                          :top-level true} opts)
             imported-vars (atom {})
             public-vars (atom #{})
             aliases (atom {core-alias cc/*core-package*})
             jsx-runtime (:jsx-runtime opts)
             jsx-dev (:development jsx-runtime)
             imports (atom (if cc/*repl*
                             (format "var %s = await import('%s');\n"
                                     core-alias cc/*core-package*)
                             (format "import * as %s from '%s';\n"
                                     core-alias cc/*core-package*)))
             pragmas (atom {:js ""})]
         (binding [*imported-vars* imported-vars
                   *public-vars* public-vars
                   *aliases* aliases
                   cc/*target* :cherry
                   *jsx* false
                   cc/*cljs-ns* (:ns opts cc/*cljs-ns*)
                   cc/*async* (:async opts)
                   cc/*cljs-ns* (:ns opts cc/*cljs-ns*)]
           (let [transpiled (transpile-internal x (assoc opts
                                                                :core-alias core-alias
                                                                :imports imports
                                                                :jsx false
                                                                :need-html-import need-html-import
                                                                :pragmas pragmas))
                 jsx *jsx*
                 _ (when (and jsx jsx-runtime)
                     (swap! imports str
                            (format
                             "var {jsx%s: _jsx, jsx%s%s: _jsxs, Fragment: _Fragment } = await import('%s');\n"
                             (if jsx-dev "DEV" "")
                             (if jsx-dev "" "s")
                             (if jsx-dev "DEV" "")
                             (str (:import-source jsx-runtime
                                                  "react")
                                  (if jsx-dev
                                    "/jsx-dev-runtime"
                                    "/jsx-runtime")))))
                 _ (when @need-html-import
                     (swap! imports str
                            (if cc/*repl*
                              "var squint_html = await import('squint-cljs/src/squint/html.js');\n"
                              "import * as squint_html from 'squint-cljs/src/squint/html.js';\n")))
                 pragmas (:js @pragmas)
                 imports (when-not elide-imports @imports)
                 exports (when-not elide-exports
                           (str (when-let [vars (disj @public-vars "default$")]
                                  (when (seq vars)
                                    (format "\nexport { %s }\n"
                                            (str/join ", " vars))))
                                (when (contains? @public-vars "default$")
                                  "export default default$\n")))]
             (assoc opts
                    :pragmas pragmas
                    :imports imports
                    :exports exports
                    :body transpiled
                    :javascript (str pragmas imports transpiled exports)
                    :jsx jsx
                    :ns cc/*cljs-ns*
                    :ns-state (:ns-state opts)))))))))

(defn compile-string*
  ([s] (compile-string* s nil))
  ([s opts] (compile-string* s opts nil))
  ([s opts state]
   (compile-internal s (merge state opts))))

(defn- symbolize-macro-config [m]
  (-> (update-keys m symbol)
      (update-vals (fn [ns]
                     (update-keys ns symbol)))))

(defn- clj-ize-opts [opts]
  (cond-> opts
    (:context opts) (update :context keyword)
    (:ns opts) (update :ns symbol)
    (:elide_imports opts) (assoc :elide-imports (:elide_imports opts))
    (:elide_exports opts) (assoc :elide-exports (:elide_exports opts))
    (:macros opts) (update :macros symbolize-macro-config)))

#?(:cljs
   (defn compileStringEx [s opts state]
     (let [opts (js->clj opts :keywordize-keys true)
           state (js->clj state :keywordize-keys true)]
       (clj->js (compile-string* s (clj-ize-opts opts) (clj-ize-opts state))))))

(defn compile-string
  ([s] (compile-string s nil))
  ([s opts]
   (let [{:keys [javascript]}
         (compile-string* s (clj-ize-opts opts))]
     javascript)))

(defn compile-form*
  ([form] (compile-form* form nil))
  ([s opts]
   (compile-internal s opts)))

(defn compile-form
  ([form] (compile-form form nil))
  ([s opts]
   (let [{:keys [imports exports body]}
         (compile-form* s opts)]
     (str imports body exports))))
