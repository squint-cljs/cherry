(ns cherry.internal.protocols
  (:require [clojure.core :as core]))

(core/defn- protocol-prefix [psym]
  (core/str (core/-> (core/str psym)
                     (.replace #?(:clj \. :cljs (js/RegExp. "\\." "g")) \$)
                     (.replace \/ \$))
            "$"))

(defn core-unchecked-get [obj key]
  (list 'js* "(~{}[~{}])" obj key))

(core/defmacro core-defprotocol
  "A protocol is a named set of named methods and their signatures:
  (defprotocol AProtocolName
    ;optional doc string
    \"A doc string for AProtocol abstraction\"
  ;method signatures
    (bar [this a b] \"bar docs\")
    (baz [this a] [this a b] [this a b c] \"baz docs\"))
  No implementations are provided. Docs can be specified for the
  protocol overall and for each method. The above yields a set of
  polymorphic functions and a protocol object. All are
  namespace-qualified by the ns enclosing the definition The resulting
  functions dispatch on the type of their first argument, which is
  required and corresponds to the implicit target object ('this' in
  JavaScript parlance). defprotocol is dynamic, has no special compile-time
  effect, and defines no new types.
  (defprotocol P
    (foo [this])
    (bar-me [this] [this y]))
  (deftype Foo [a b c]
    P
    (foo [this] a)
    (bar-me [this] b)
    (bar-me [this y] (+ c y)))
  (bar-me (Foo. 1 2 3) 42)
  => 45
  (foo
    (let [x 42]
      (reify P
        (foo [this] 17)
        (bar-me [this] x)
        (bar-me [this y] x))))
  => 17"
  [psym & doc+methods]
  (core/let [p psym #_(:name (cljs.analyzer/resolve-var (dissoc &env :locals) psym))
             [opts methods]
             (core/loop [opts {:protocol-symbol true}
                         methods []
                         sigs doc+methods]
               (core/if-not (seq sigs)
                 [opts methods]
                 (core/let [[head & tail] sigs]
                   (core/cond
                     (core/string? head)
                     (recur (assoc opts :doc head) methods tail)
                     (core/keyword? head)
                     (recur (assoc opts head (first tail)) methods (rest tail))
                     (core/seq? head)
                     (recur opts (conj methods head) tail)
                     :else
                     (throw #?(:clj  (Exception.
                                       (core/str "Invalid protocol, " psym " received unexpected argument"))
                               :cljs (js/Error.
                                       (core/str "Invalid protocol, " psym " received unexpected argument"))))
                     ))))
             psym (vary-meta psym merge opts)
             ns-name (core/-> &env :ns :name)
             ;; TODO
             fqn (core/fn [n] (symbol nil #_(core/str ns-name) (core/str n)))
             prefix (protocol-prefix p)
             _ (core/doseq [[mname & arities] methods]
                 (core/when (some #{0} (map count (filter vector? arities)))
                   (throw
                     #?(:clj (Exception.
                               (core/str "Invalid protocol, " psym
                                 " defines method " mname " with arity 0"))
                        :cljs (js/Error.
                                (core/str "Invalid protocol, " psym
                                  " defines method " mname " with arity 0"))))))
             sig->syms (core/fn [sig]
                         (core/if-not (every? core/symbol? sig)
                           (mapv (core/fn [arg]
                                   (core/cond
                                     (core/symbol? arg) arg
                                     (core/and (map? arg) (core/some? (:as arg))) (:as arg)
                                     :else (gensym))) sig)
                           sig))
             expand-dyn (core/fn [fname sig]
                          (core/let [sig (sig->syms sig)

                                     fqn-fname (with-meta (fqn fname) {:cljs.analyzer/no-resolve true})
                                     fsig (first sig)

                                     ;; construct protocol checks in reverse order
                                     ;; check the.protocol/fn["_"] for default impl last
                                     check
                                     `(let [m# (unchecked-get ~fqn-fname "_")]
                                        (if-not (nil? m#)
                                          (m# ~@sig)
                                          (throw
                                            (missing-protocol
                                              ~(core/str psym "." fname) ~fsig))))

                                     ;; then check protocol on js string,function,array,object (first dynamic check actually executed)
                                     check
                                     `(let [x# (if (nil? ~fsig) nil ~fsig)
                                            m# (unchecked-get ~fqn-fname (goog/typeOf x#))]
                                        (if-not (nil? m#)
                                          (m# ~@sig)
                                          ~check))]
                            `(~sig ~check)))
             expand-sig (core/fn [fname dyn-name slot sig]
                          (core/let [sig (sig->syms sig)

                                     fqn-fname (with-meta (fqn fname) {:cljs.analyzer/no-resolve true})
                                     fsig (first sig)

                                     ;; check protocol property on object (first check executed)
                                     check
                                     `(if (and (not (nil? ~fsig))
                                               ;; Property access needed here.
                                               (not (nil? (. ~fsig ~(with-meta (symbol (core/str "-" slot)) {:protocol-prop true})))))
                                        (. ~fsig ~slot ~@sig)
                                        (~dyn-name ~@sig))

                                     ;; then check protocol fn in metadata (only when protocol is marked with :extend-via-metadata true)
                                     check
                                     (core/if-not (:extend-via-metadata opts)
                                       check
                                       `(if-let [meta-impl# (-> ~fsig (core/meta) (core/get '~fqn-fname))]
                                          (meta-impl# ~@sig)
                                          ~check))]
                            `(~sig ~check)))
             psym (core/-> psym
                    (vary-meta update-in [:jsdoc] conj "@interface")
                    (vary-meta assoc-in [:protocol-info :methods]
                      (into {}
                        (map
                          (core/fn [[fname & sigs]]
                            (core/let [doc (core/as-> (last sigs) doc
                                             (core/when (core/string? doc) doc))
                                       sigs (take-while vector? sigs)]
                              [(vary-meta fname assoc :doc doc)
                               (vec sigs)]))
                          methods)))
                    ;; for compatibility with Clojure
                    (vary-meta assoc-in [:sigs]
                      (into {}
                        (map
                          (core/fn [[fname & sigs]]
                            (core/let [doc (core/as-> (last sigs) doc
                                             (core/when (core/string? doc) doc))
                                       sigs (take-while vector? sigs)]
                              [(keyword fname) {:name fname :arglists (list* sigs) :doc doc}]))
                          methods))))
             method (core/fn [[fname & sigs]]
                      (core/let [doc (core/as-> (last sigs) doc
                                       (core/when (core/string? doc) doc))
                                 sigs (take-while vector? sigs)
                                 #_#_amp (core/when (some #{'&} (apply concat sigs))
                                       (cljs.analyzer/warning
                                        :protocol-with-variadic-method
                                        &env {:protocol psym :name fname}))
                                 _ (core/when-some [existing (core/get (-> &env :ns :defs) fname)]
                                     #_(core/when-not (= p (:protocol existing))
                                       (cljs.analyzer/warning
                                         :protocol-with-overwriting-method
                                         {} {:protocol psym :name fname :existing existing})))
                                 slot (symbol (core/str prefix (munge (name fname))))
                                 dyn-name (symbol (core/str slot "$dyn"))
                                 fname (vary-meta fname assoc
                                         :protocol p
                                         :doc doc)]
                        `(let [~dyn-name (core/fn
                                           ~@(map (core/fn [sig]
                                                    (expand-dyn fname sig))
                                               sigs))]
                           (defn ~fname
                             ~@(map (core/fn [sig]
                                      (expand-sig fname dyn-name
                                        (with-meta (symbol (core/str slot "$arity$" (count sig)))
                                          {:protocol-prop true})
                                        sig))
                                    sigs)))))
             ret `(do
                    #_(set! ~'*unchecked-if* true)
                    (def ~psym (~'js* "function(){}"))
                    ~@(map method methods)
                    #_(set! ~'*unchecked-if* false))]
    (prn ret)
    ret))
