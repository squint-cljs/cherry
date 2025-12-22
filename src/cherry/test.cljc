(ns cherry.test)

(defn assert-expr [msg form]
  (let [op (when (sequential? form) (first form))
        loc (meta form)
        line (:line loc)
        column (:column loc)
        report (fn [type expected actual ret]
                 `(do (clojure.test/report {:type ~type :message ~msg :expected ~expected :actual ~actual
                                            ~@(when line [:line line])
                                            ~@(when column [:column column])})
                      ~ret))
        default (let [sym (gensym "value")]
                  `(let [~sym ~form]
                     (if ~sym
                       ~(report :pass `'~form sym sym)
                       ~(report :fail `'~form sym sym))))]
    (case op
      = (if (= 2 (count (rest form)))
          (let [[expected actual] (rest form)
                expected-sym (gensym "expected")
                actual-sym (gensym "actual")
                result-sym (gensym "result")]
            `(let [~expected-sym ~expected
                   ~actual-sym ~actual
                   ~result-sym (= ~expected-sym ~actual-sym)]
               (if ~result-sym
                 ~(report :pass expected-sym actual-sym true)
                 ~(report :fail expected-sym actual-sym false))))
          default)
      thrown? (let [klass (second form)
                    body (nthnext form 2)
                    e-sym (gensym "e")]
                `(try
                   (do ~@body)
                   ~(report :fail `'~form "No exception thrown" false)
                   (catch :default ~e-sym
                     (if (instance? ~klass ~e-sym)
                       ~(report :pass `'~form e-sym true)
                       ~(report :fail `'~form e-sym false)))))
      thrown-with-msg? (let [klass (second form)
                             re (nth form 2)
                             body (nthnext form 3)
                             e-sym (gensym "e")]
                         `(try
                            (do ~@body)
                            ~(report :fail `'~form "No exception thrown" false)
                            (catch :default ~e-sym
                              (if (instance? ~klass ~e-sym)
                                (if (re-find ~re (.-message ~e-sym))
                                  ~(report :pass `'~form e-sym true)
                                  ~(report :fail `'~form `(str "Exception message \"" (.-message ~e-sym) "\" did not match " ~re) false))
                                ~(report :fail `'~form e-sym false)))))
      default)))

(defmacro deftest [name & body]
  (let [fn-meta (select-keys (meta name) [:async])]
    `(def ~(vary-meta name assoc :test true)
       (with-meta ~(with-meta `(fn [] ~@body) fn-meta) {:name '~name}))))

(defmacro is
  ([form] `(is ~form nil))
  ([form msg]
   (let [loc (meta &form)
         form-with-meta (if (and loc (or (sequential? form) (symbol? form)))
                          (with-meta form loc)
                          form)]
     (assert-expr msg form-with-meta))))

(defmacro testing [string & body]
  `(do
     (clojure.test/update-current-env! [:testing-contexts] conj ~string)
     (try
       ~@body
       (finally
         (clojure.test/update-current-env! [:testing-contexts] rest)))))

(defmacro deftest- [name & body]
  `(deftest ~(vary-meta name assoc :private true) ~@body))

(defmacro are [bindings expr & args]
  (assert (pos? (count bindings)) "are requires at least one binding")
  (assert (seq args) "are requires at least one test case")
  (let [binding-count (count bindings)]
    (assert (zero? (mod (count args) binding-count))
            (str "are: arg count (" (count args) ") must be divisible by binding count (" binding-count ")"))
    `(do ~@(for [arg-group (partition binding-count args)]
             `(clojure.test/is (let [~@(interleave bindings arg-group)] ~expr))))))
