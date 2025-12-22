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
