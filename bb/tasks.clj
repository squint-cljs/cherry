(ns tasks
  (:require
   [babashka.fs :as fs]
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
        reserved (edn/read-string (slurp (io/resource "cherry/js_reserved.edn")))]
    {:modules
     {:cljs.core {:exports (assoc (->namespace "cljs.core" (:vars core-config) reserved)
                                  'goog_typeOf 'goog/typeOf)}
      :clojure.string {:exports (->namespace "clojure.string" (:vars string-config) reserved)
                       :entries '[clojure.string]
                       :depends-on #{:cljs.core}}
      :clojure.walk {:exports (->namespace "clojure.walk" (:vars walk-config) reserved)
                     :entries '[clojure.walk]
                     :depends-on #{:cljs.core}}}}))

(def test-config
  '{:compiler-options {:load-tests true}
    :modules {:cherry.tests {:init-fn cherry.compiler-test/init
                             :depends-on #{:node
                                           :compiler
                                           :cljs.core
                                           :clojure.string
                                           :clojure.walk}}}})

(defn shadow-extra-test-config []
  (merge-with
   merge
   (shadow-extra-config)
   test-config))

(defn build-cherry-npm-package []
  (fs/create-dirs ".work")
  (fs/delete-tree "lib")
  (fs/delete-tree ".shadow-cljs")
  (spit ".work/config-merge.edn" (shadow-extra-config))
  (shell "npx shadow-cljs --config-merge .work/config-merge.edn release cherry"))

(defn publish []
  (build-cherry-npm-package)
  (run! fs/delete (fs/glob "lib" "*.map"))
  (shell "esbuild index.js --bundle --minify --format=iife --global-name=CherryCljs --outfile=lib/cherry.umd.js")
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
