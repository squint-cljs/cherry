(ns cherry.internal.cli
  (:require
   ["path" :as path]
   [cherry.compiler :as cc]
   [cherry.compiler.node :as compiler]
   [clojure.string :as str]
   [squint.internal.cli-common :as cli-common]
   [squint.internal.node.utils :as utils]))

(defn file-in-output-dir [file paths output-dir]
  (if output-dir
    (path/resolve output-dir
                  (utils/adjust-file-for-paths file paths))
    file))

(defn resolve-ns [opts in-file x]
  (let [output-dir (:output-dir opts)
        paths (:paths opts)
        in-file-in-output-dir (file-in-output-dir in-file paths output-dir)]
    (when-let [resolved
               (some-> (utils/resolve-file x (:paths opts ["." "src"]))
                       (file-in-output-dir paths output-dir)
                       (some->> (path/relative (path/dirname (str in-file-in-output-dir)))))]
      (let [ext (:extension opts ".mjs")
            ext (if (str/starts-with? ext ".")
                  ext
                  (str "." ext))
            ext' (path/extname resolved)
            file (str "./" (str/replace resolved (re-pattern (str ext' "$")) ext))]
        file))))

(def dialect
  {:prog "cherry"
   :log-prefix "[cherry]"
   :config-file compiler/config-file
   :compile-file compiler/compile-file
   :compile-string cc/compile-string
   :resolve-ns resolve-ns
   :eval-tmp-file "cherry.mjs"})

(def table
  [(cli-common/run-cmd dialect)
   (cli-common/compile-cmd dialect)
   (cli-common/watch-cmd dialect)
   (cli-common/eval-cmd dialect)])

(defn init []
  (cli-common/init dialect table))
