(ns tasks
  (:require
   [babashka.fs :as fs]
   [babashka.http-server :as server]
   [babashka.process :refer [shell]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn munge* [s reserved]
  (let [s (str (munge s))]
    (if (contains? reserved s)
      (str s "$")
      s)))

(defn- dynvar? [v]
  (let [s (str v)]
    (and (str/starts-with? s "_STAR_")
         (str/ends-with? s "_STAR_"))))

(defn ->namespace [the-ns-name vars reserved]
  (let [ks (map #(symbol (munge* % reserved)) vars)
        ;; earmuffed vars export their accessor box (see cherry.dynvars)
        vs (map #(if (and (= "cljs.core" the-ns-name) (dynvar? %))
                   (symbol "cherry.dynvars" (str %))
                   (symbol (str the-ns-name) (str %)))
                vars)
        the-map (zipmap ks vs)]
    the-map))

(defn shadow-extra-config
  []
  (let [core-config (edn/read-string (slurp (io/resource "cherry/cljs.core.edn")))
        string-config (edn/read-string (slurp (io/resource "cherry/clojure.string.edn")))
        walk-config (edn/read-string (slurp (io/resource "cherry/clojure.walk.edn")))
        set-config (edn/read-string (slurp (io/resource "cherry/clojure.set.edn")))
        pprint-config (edn/read-string (slurp (io/resource "cherry/clojure.pprint.edn")))
        test-config (edn/read-string (slurp (io/resource "cherry/cherry.test.edn")))
        reserved (edn/read-string (slurp (io/resource "cherry/js_reserved.edn")))]
    {:modules
     {:cljs.core {:entries '[cherry.dynvars]
                  :exports (assoc (->namespace "cljs.core" (:vars core-config) reserved)
                                  'goog_typeOf 'goog/typeOf)}
      :clojure.string {:exports (->namespace "clojure.string" (:vars string-config) reserved)
                       :entries '[clojure.string]
                       :depends-on #{:cljs.core}}
      :clojure.walk {:exports (->namespace "clojure.walk" (:vars walk-config) reserved)
                     :entries '[clojure.walk]
                     :depends-on #{:cljs.core}}
      :clojure.set {:exports (->namespace "clojure.set" (:vars set-config) reserved)
                    :entries '[clojure.set]
                    :depends-on #{:cljs.core}}
      :cljs.pprint {:exports (->namespace "cljs.pprint" (:vars pprint-config) reserved)
                    :entries '[cljs.pprint]
                    :depends-on #{:cljs.core :clojure.string}}
      :clojure.test {:exports (->namespace "cherry.test" (:vars test-config) reserved)
                     :entries '[cherry.test]
                     :depends-on #{:cljs.core :clojure.string}}}}))

(def test-config
  '{:compiler-options {:load-tests true}
    :modules {:cherry.tests {:init-fn cherry.compiler-test/init
                             :depends-on #{:node
                                           :compiler
                                           :cljs.core
                                           :clojure.string
                                           :clojure.walk
                                           :cljs.pprint}}}})

(defn shadow-extra-test-config []
  (merge-with
   merge
   (shadow-extra-config)
   test-config))

(defn patch-protocol-sentinel
  "Rewrites the PROTOCOL_SENTINEL init in lib/cljs.core.js to share one
  sentinel with other CLJS runtimes in the same JS realm. See #190."
  []
  (let [f (fs/file "lib" "cljs.core.js")
        s (slurp f)
        [_ munged] (re-find #"PROTOCOL_SENTINEL=([\w$.]+)" s)
        _ (when-not munged
            (throw (ex-info "no PROTOCOL_SENTINEL export in lib/cljs.core.js" {})))
        init (str munged "={}")
        parts (str/split s (re-pattern (java.util.regex.Pattern/quote init)) -1)
        _ (when-not (= 2 (count parts))
            (throw (ex-info "expected one PROTOCOL_SENTINEL init site"
                            {:munged munged :sites (dec (count parts))})))
        ;; every property stamped with the sentinel is a protocol marker and
        ;; part of cherry's ABI: it must survive Closure renaming
        marker-re (re-pattern (str "\\.([A-Za-z_$][A-Za-z0-9_$]*)="
                                   (java.util.regex.Pattern/quote munged)
                                   "(?![=A-Za-z0-9_$])"))
        renamed (into (sorted-set)
                      (keep (fn [[_ prop]]
                              (when-not (str/starts-with? prop "cljs$")
                                prop)))
                      (re-seq marker-re s))
        _ (when (seq renamed)
            (throw (ex-info "protocol markers renamed by Closure, run bb gen-externs"
                            {:props (vec renamed)})))
        shared (str munged
                    "=(()=>{let g=globalThis,c=g.cljs||(g.cljs={}),o=c.core||(c.core={});"
                    "return o.PROTOCOL_SENTINEL||(o.PROTOCOL_SENTINEL=Date.prototype.cljs$core$IEquiv$||{})})()")]
    (spit f (str/join shared parts))))

(defn build-cherry-npm-package []
  (fs/create-dirs ".work")
  (fs/delete-tree "lib")
  (fs/delete-tree ".shadow-cljs")
  (spit ".work/config-merge.edn" (shadow-extra-config))
  (shell "npx shadow-cljs --config-merge .work/config-merge.edn release cherry")
  (patch-protocol-sentinel))

(defn publish []
  (build-cherry-npm-package)
  (run! fs/delete (fs/glob "lib" "*.map"))
  (shell "npx esbuild index.js --bundle --minify --format=iife --global-name=CherryCljs --outfile=lib/cherry.umd.js --ignore-annotations")
  (shell "npm publish"))

(defn watch-cherry []
  (fs/delete-tree ".shadow-cljs/builds/clava/dev/ana/cherry")
  (fs/create-dirs ".work")
  (spit ".work/config-merge.edn" (shadow-extra-test-config))
  (shell "npx shadow-cljs --config-merge .work/config-merge.edn watch cherry"))

(defn test-cljs-test []
  ;; Compiled with test-resources as the working dir so the :require-macros
  ;; companion (cljs_test_smoke_macros.cljc) resolves on the default path.
  (let [dir "test-resources"
        out-file (str dir "/cljs_test_smoke.mjs")
        macros-out (str dir "/cljs_test_smoke_macros.mjs")]
    (shell {:dir dir} "node" "../node_cli.js" "compile" "cljs_test_smoke.cljs")
    (let [out (:out (shell {:dir dir :out :string} "node" "cljs_test_smoke.mjs"))]
      (fs/delete out-file)
      (when (fs/exists? macros-out) (fs/delete macros-out))
      (assert (str/includes? out "Ran 13 tests containing 26 assertions") out)
      (assert (str/includes? out "1 failures, 0 errors") out)
      ;; :begin-test-ns must fire for each ns visited by run-tests
      (assert (str/includes? out "Testing ns.a") out)
      (assert (str/includes? out "Testing ns.b") out)
      ;; (run-tests 'synthetic.ns) macro must compile to a string lookup
      (assert (str/includes? out "Testing synthetic.ns") out))))

(defn test-cherry []
  (fs/create-dirs ".work")
  (spit ".work/config-merge.edn" (shadow-extra-test-config))
  (shell "npm install")
  (shell "npx shadow-cljs --config-merge .work/config-merge.edn release cherry")
  (patch-protocol-sentinel)
  (shell "node lib/cherry.tests.js")
  (shell "node test-resources/sentinel/host_first.mjs")
  (shell "node test-resources/sentinel/cherry_first.mjs")
  (shell "node test-resources/sentinel/advanced_host_first.mjs")
  (test-cljs-test))

(defn- compile-e2e []
  (shell "node" "node_cli.js" "compile"
         "--paths" "e2e"
         "--output-dir" "e2e"
         "--extension" "mjs"))

(defn e2e
  "Browser REPL e2e: `vite dev` against examples/browser-repl (isolated ports),
  headless playwright browser and an nREPL client."
  []
  (compile-e2e)
  (shell {:dir "examples/browser-repl"} "npm" "install")
  (shell "node" "e2e/browser_repl_test.mjs"))

(defn bump-compiler-common []
  (let [{:keys [out]}
        (shell {:out :string
                :dir "../squint/compiler-common"} "git rev-parse HEAD")
        sha (str/trim out)
        deps (slurp "deps.edn")
        nodes ((requiring-resolve 'borkdude.rewrite-edn/parse-string) deps)
        nodes ((requiring-resolve 'borkdude.rewrite-edn/assoc-in) nodes [:deps 'io.github.squint-cljs/compiler-common :git/sha] sha)
        deps (str nodes)]
    (spit "deps.edn" deps)))

(defn start-playground-server [opts]
  (server/exec (merge {:dir "."}
                      opts)))
