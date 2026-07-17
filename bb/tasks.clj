(ns tasks
  (:require
   [babashka.fs :as fs]
   [babashka.http-server :as server]
   [babashka.process :refer [shell]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [rewrite-clj.zip :as z]))

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

;; --- Patch cljs/core.cljs for the source-override build ----------------------
;; Two structural (rewrite-clj) edits to a build-time copy of cljs/core.cljs — see
;; ensure-patched-core! for why it's a classpath override in the first place:
;;
;; 1. Share PROTOCOL_SENTINEL. cljs.core defines `(defonce PROTOCOL_SENTINEL #js {})` — the
;;    identity token used to mark protocol impls (including those extended onto native
;;    prototypes like js/Date) and to dispatch on them (`SENTINEL === x.cljs$core$IComparable$`).
;;    Compiled as an isolated ESM module it always mints a *fresh* object, so when Cherry output
;;    shares a realm with another cljs runtime (e.g. shadow-cljs) the later-loaded one overwrites
;;    the shared prototypes' marker with its own sentinel, and the other runtime's
;;    satisfies?/compare/= on native types then throw. We rewrite the init to reuse — and
;;    publish — a shared global cljs.core.PROTOCOL_SENTINEL, the way a browser/goog build shares
;;    it across compilation units. https://github.com/squint-cljs/cherry/issues/190
;;
;; 2. Add `(set! *warn-on-infer* false)`. Overriding cljs.core as classpath source knocks it off
;;    the jar's AOT-analysis-cache path, so `:infer-externs :auto` re-analyzes it AS SOURCE and
;;    surfaces core's ~150 pre-existing, benign infer-warnings (core has 400+ untyped field
;;    accesses and zero ^js hints — it relies on warn-on-infer being off). We add the file-scope
;;    directive core is designed around, so the recompiled-as-source core behaves like the
;;    AOT-cached one. NB: this is a source edit, NOT a change to the build's compiler-options.

(def ^:private sentinel-guard-src
  ;; Bracket/string property access is deliberate: Closure :advanced renames dotted
  ;; `.cljs`/`.core` access (not in externs), which would point the guard at the wrong
  ;; global path; quoted keys are never renamed.
  (let [br (fn [k] (str "[\\\"" k "\\\"]"))]
    (str "(js* \"(function(){"
         "var g=typeof globalThis!=='undefined'?globalThis:typeof self!=='undefined'?self:this,"
         "c=g" (br "cljs") "||(g" (br "cljs") "={}),"
         "cc=c" (br "core") "||(c" (br "core") "={});"
         "return cc" (br "PROTOCOL_SENTINEL") "||(cc" (br "PROTOCOL_SENTINEL") "={})})()\")")))

(defn- top-level-def?
  "True when zloc is a top-level (<head> <sym> …) with head ∈ `heads` and `sym` in DEFINITION
   position (the name) — not a usage or a value in some other form. Only ever calls z/sexpr on
   atomic tokens (never on #js {} etc., which has no bb data-reader)."
  [zloc heads sym]
  (boolean
   (and (= :list (z/tag zloc))
        (when-let [head (z/down zloc)]
          (and (= :token (z/tag head))
               (contains? heads (z/sexpr head))
               (when-let [nm (z/right head)]
                 (= sym (case (z/tag nm)
                          :token (z/sexpr nm)
                          :meta  (-> nm z/down z/rightmost z/sexpr) ;; symbol under ^metadata
                          nil))))))))

(defn- find-def
  "Zipper at the single top-level form defining `sym` with a head in `heads`. Throws unless
   there is EXACTLY ONE, so we can never touch the wrong node."
  [src heads sym]
  (let [hits (loop [zloc (z/of-string src) acc []]
               (if (or (nil? zloc) (z/end? zloc))
                 (if (and zloc (top-level-def? zloc heads sym)) (conj acc zloc) acc)
                 (recur (z/right zloc) (if (top-level-def? zloc heads sym) (conj acc zloc) acc))))]
    (when (not= 1 (count hits))
      (throw (ex-info (str "cljs.core: expected exactly one " (pr-str (vec heads)) " of " sym
                           ", found " (count hits))
                      {:sym sym :count (count hits)})))
    (first hits)))

(defn patch-core
  "cljs/core.cljs source with the two edits above applied."
  [src]
  (let [src (let [init (-> (find-def src #{'def 'defonce} 'PROTOCOL_SENTINEL) z/down z/rightmost)]
              (when-not (= "#js {}" (str/trim (z/string init)))
                (throw (ex-info (str "cljs.core: unexpected PROTOCOL_SENTINEL init " (pr-str (z/string init)))
                                {:init (z/string init)})))
              (-> init (z/replace (z/node (z/of-string sentinel-guard-src))) z/root-string))]
    (-> (find-def src #{'def} '*warn-on-infer*)
        (z/insert-right (z/node (z/of-string "(set! *warn-on-infer* false)")))
        (z/insert-newline-right)
        z/root-string)))

(defn- clojurescript-jar []
  (let [sep (java.io.File/pathSeparator)
        cp (str/trim (:out (shell {:out :string} "clojure" "-A:cljs" "-Spath")))]
    (or (->> (str/split cp (re-pattern (java.util.regex.Pattern/quote sep)))
             (filter #(re-find #"clojurescript-[0-9][^/\\]*\.jar$" %))
             first)
        (throw (ex-info "clojurescript jar not found on the :cljs classpath" {})))))

(defn- jar-entry [jar entry]
  (with-open [zf (java.util.zip.ZipFile. jar)]
    (if-let [e (.getEntry zf entry)]
      (slurp (.getInputStream zf e))
      (throw (ex-info (str entry " not found in " jar) {})))))

;; Extract cljs/core.cljs from the ClojureScript jar shadow will compile, rewrite its
;; PROTOCOL_SENTINEL, and drop it under .work/cljs-patch (which the :build-core-patch alias
;; puts on the classpath AHEAD of the jar, so shadow compiles this copy). Self-syncing: it
;; patches whatever version the jar ships, so it can't drift.
(defn ensure-patched-core! []
  (let [patched (patch-core (jar-entry (clojurescript-jar) "cljs/core.cljs"))]
    (fs/create-dirs ".work/cljs-patch/cljs")
    (spit ".work/cljs-patch/cljs/core.cljs" patched)))

(defn build-cherry-npm-package []
  (fs/create-dirs ".work")
  (fs/delete-tree "lib")
  (fs/delete-tree ".shadow-cljs")
  (ensure-patched-core!)
  (spit ".work/config-merge.edn" (shadow-extra-config))
  (shell "npx shadow-cljs --config-merge .work/config-merge.edn release cherry"))

(defn publish []
  (build-cherry-npm-package)
  (run! fs/delete (fs/glob "lib" "*.map"))
  (shell "npx esbuild index.js --bundle --minify --format=iife --global-name=CherryCljs --outfile=lib/cherry.umd.js --ignore-annotations")
  (shell "npm publish"))

(defn watch-cherry []
  (fs/delete-tree ".shadow-cljs/builds/clava/dev/ana/cherry")
  (fs/create-dirs ".work")
  (ensure-patched-core!)
  (spit ".work/config-merge.edn" (shadow-extra-test-config))
  (shell "npx shadow-cljs --config-merge .work/config-merge.edn watch cherry"))

(defn test-cherry []
  (fs/create-dirs ".work")
  (ensure-patched-core!)
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
