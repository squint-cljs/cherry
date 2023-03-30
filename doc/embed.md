# Embedding cherry

Cherry can be embedded into an existing vanilla CLJS or shadow-cljs project.
To facilitate this, cherry exposes the `cherry.embed` namespace. It contains the following:

- `preserve-ns`: a macro to define a CLJS namespace as globals, so cherry output
  can access functions after advanced compilation.
- `eval-form`, `eval-string`, `compile-form` and `compile-string` functions to evaluate or compile an s-expr or string

An example shadow-cljs project:

`deps.edn`:

``` clojure
{:aliases {:cherry {:extra-deps {io.github.squint-cljs/cherry
                                 {:git/sha "d5fab9ea4e728c926a9dfa8704ff354df8cf3860"}}}
           :shadow {:extra-deps {thheller/shadow-cljs {:mvn/version "2.22.9"}}
                    :main-opts ["-m" "shadow.cljs.devtools.cli"]}}}
```

`shadow-cljs.edn`
``` clojure
{:deps true
 :builds
 {:cli
  {:target :esm
   :runtime :node
   :output-dir "out"
   :modules {:eval {:init-fn my.cherry/init}}
   :build-hooks [(shadow.cljs.build-report/hook
                  {:output-to "report.html"})]}}}

```

`src/my/cherry.cljs`:
``` clojure
(ns my.cherry
  (:require [cherry.embed :as cherry]))

(cherry/preserve-ns 'cljs.core)
(cherry/preserve-ns 'clojure.string)

(defn init []
  (let [[_ expr] (.slice js/process.argv 2)]
    (cherry/eval-string expr)))
```

When compiling this with `clojure -M:cherry:shadow release cli`, we get a single
`out/eval.js` file of about 550kb of which roughly 330kb comes from preserving
the `cljs.core` namespace.

We can invoke it with:

``` shell
$ node out/eval.js -e '(prn :hello)'
:hello
```

