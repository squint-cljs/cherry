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

(defn ->namespace [the-ns-name vars reserved]
  (let [ks (map #(symbol (munge* % reserved)) vars)
        vs (map #(symbol (str the-ns-name) (str %)) vars)
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
     {:cljs.core {:exports (assoc (->namespace "cljs.core" (:vars core-config) reserved)
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

;; cljs.core (1.10.439) defines `(defonce PROTOCOL_SENTINEL #js {})` — the protocol
;; identity token used to mark, and dispatch on, protocol impls (incl. those extended
;; onto native prototypes like js/Date). Built as an isolated ESM module, that `defonce`
;; always mints a *fresh* object, and the guarded init newer ClojureScript uses to share
;; one sentinel across independently-compiled runtimes in a realm is absent. So when
;; Cherry output runs alongside another cljs runtime (e.g. shadow-cljs) on the same page,
;; whichever loads last overwrites the shared prototypes' marker with its own sentinel,
;; and the other runtime's `satisfies?`/`compare`/`=` on natives then throw. Reconcile by
;; reusing a shared global `cljs.core.PROTOCOL_SENTINEL` (publishing ours if none exists),
;; mirroring newer ClojureScript. See https://github.com/squint-cljs/cherry/issues/190
(defn share-protocol-sentinel!
  [file]
  (let [src (slurp file)
        ref (second (re-find #"export const PROTOCOL_SENTINEL=(\$APP\.\w+);" src))
        _ (when-not ref
            (throw (ex-info "PROTOCOL_SENTINEL export not found — cljs.core emit changed" {:file file})))
        needle (str ref "={}")
        replacement (str ref "=(function(){var g=typeof globalThis!==\"undefined\"?globalThis:"
                         "typeof self!==\"undefined\"?self:this,c=g.cljs||(g.cljs={}),"
                         "cc=c.core||(c.core={});return cc.PROTOCOL_SENTINEL||(cc.PROTOCOL_SENTINEL={})})()")]
    (when-not (str/includes? src needle)
      (throw (ex-info (str "sentinel init '" needle "' not found") {:file file})))
    (spit file (str/replace-first src needle replacement))))

(defn build-cherry-npm-package []
  (fs/create-dirs ".work")
  (fs/delete-tree "lib")
  (fs/delete-tree ".shadow-cljs")
  (spit ".work/config-merge.edn" (shadow-extra-config))
  (shell "npx shadow-cljs --config-merge .work/config-merge.edn release cherry")
  (share-protocol-sentinel! "lib/cljs.core.js"))

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

(defn test-cherry []
  (fs/create-dirs ".work")
  (spit ".work/config-merge.edn" (shadow-extra-test-config))
  (shell "npm install")
  (shell "npx shadow-cljs --config-merge .work/config-merge.edn release cherry")
  (shell "node lib/cherry.tests.js"))

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
