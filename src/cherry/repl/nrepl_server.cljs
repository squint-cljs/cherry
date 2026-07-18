(ns cherry.repl.nrepl-server
  "Cherry's nREPL server: squint.repl.nrepl-server-common with cherry's
  compiler. The shadow :node.nrepl_server module exports startServer,
  handleBrowserMessage and evalString from here. Eval'd code and this bundle
  share one cljs.core module instance, so pr-str prints eval results."
  (:require
   [cherry.compiler :as compiler]
   [cherry.compiler.node :as compiler-node]
   [squint.repl.nrepl-server-common :as common]
   [squint.repl.print-common :as print-common]))

(common/set-dialect!
 {:compile-string* compiler/compile-string*
  :resolve-ns-repl compiler-node/resolve-ns-repl
  :config-file compiler-node/config-file
  :pr-str-repl (fn [v] (print-common/pr-str-repl pr-str v))})

(def start-server common/start-server)
(def handle-browser-message common/handle-browser-message)
(def evaluate-string common/evaluate-string)
