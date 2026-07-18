(ns cherry.compiler.sci
  (:require ["fs" :as fs]
            ["module" :refer [createRequire]]
            ["path" :as path]
            ["url" :refer [pathToFileURL]]
            [cherry.compiler.node :as cn :refer [sci]]
            [sci.core :as sci]
            [squint.internal.node.utils :as utils]))

(defn slurp [f]
  (fs/readFileSync f "utf-8"))

(def cwd (.-cwd js/process))

(def require-from-cwd
  (createRequire
   (-> (path/join (cwd) "noop.js")
       pathToFileURL
       .-href)))

(defn- resolve-macro-file [namespace]
  (utils/resolve-file namespace (:paths (utils/get-cfg cn/config-file) ["." "src"])))

(declare ctx)
(def ctx (sci/init {:load-fn (fn [{:keys [namespace]}]
                               (if (string? namespace)
                                 (let [mod (require-from-cwd namespace)]
                                   (sci/add-js-lib! ctx namespace mod)
                                   ;; empty map = SCI will take care of aliases, refer, etc.
                                   {})
                                 (when-let [f (resolve-macro-file namespace)]
                                   (let [fstr (slurp f)]
                                     ;; :file lets SCI bind *file* so an error
                                     ;; while loading this ns names its source.
                                     {:file f
                                      :source fstr}))))
                    :classes {:allow :all
                              'js js/globalThis}
                    :features #{:cherry :cljs}
                    :unrestricted true}))

(sci/alter-var-root sci/print-fn (constantly *print-fn*))
(sci/alter-var-root sci/print-err-fn (constantly *print-err-fn*))

(defn init []
  (reset! sci {:resolve-file resolve-macro-file
               :eval-form (fn [form _cfg]
                            (sci/eval-form ctx form))}))
