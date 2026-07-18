(ns cherry.internal.cli
  (:require
   [cherry.compiler :as cc]
   [cherry.compiler.node :as compiler]
   [squint.internal.cli-common :as cli-common]))

(def dialect
  {:prog "cherry"
   :log-prefix "[cherry]"
   :config-file compiler/config-file
   :compile-file compiler/compile-file
   :compile-string cc/compile-string
   ;; same resolution compile-file uses for callers that don't supply :resolve-ns
   :resolve-ns compiler/resolve-ns
   :eval-tmp-file "cherry.mjs"})

(def table
  [(cli-common/run-cmd dialect)
   (cli-common/compile-cmd dialect)
   (cli-common/watch-cmd dialect)
   (cli-common/nrepl-server-cmd dialect)
   (cli-common/eval-cmd dialect)])

(defn init []
  (cli-common/init dialect table))
