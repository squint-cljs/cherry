## Cherry :cherries:

Experimental ClojureScript to ES6 module compiler.

Reducing friction between ClojureScript and JS tooling.

> :warning: This project is an experiment and not recommended to be used in
> production. It currently has many bugs and will undergo many breaking changes.

Also check out [Squint](https://github.com/squint-cljs/squint) which
is a CLJS _syntax_ to JS compiler.

## Quickstart

Although it's early days and far from complete, you're welcome to try out cherry and submit issues.

``` shell
$ mkdir cherry-test && cd cherry-test
$ npm init -y
$ npm install cherry-cljs@latest
```

Create a `.cljs` file, e.g. `example.cljs`:

``` clojure
(ns example
  (:require ["fs" :as fs]
            ["url" :refer [fileURLToPath]]))

(prn (fs/existsSync (fileURLToPath js/import.meta.url)))

(defn foo [{:keys [a b c]}]
  (+ a b c))

(js/console.log (foo {:a 1 :b 2 :c 3}))
```

Then compile and run (`run` does both):

```
$ npx cherry run example.cljs
true
6
```

Run `npx cherry --help` to see all command line options.

## Examples

A few examples of currenly working projects compiled by cherry:

- [Cherry ClojureScript template on livecodes.io](https://livecodes.io/?template=clojurescript)
- [playground](https://squint-cljs.github.io/cherry/)
- [wordle](https://squint-cljs.github.io/cherry/examples/wordle/index.html)
- [react](https://squint-cljs.github.io/cherry/examples/react/index.html)
- [vite](examples/vite)
- [browser-repl](examples/browser-repl): vite + browser REPL, three rendering models
- [replicant](examples/replicant): tic-tac-toe with replicant hiccup
- [cherry-action-example](https://github.com/borkdude/cherry-action-example)

See the [examples](examples) directory for more.

## Project goals

Goals of cherry:

- Compile `.cljs` files on the fly into ES6-compatible `.mjs` files.
- Compiler will be available on NPM and can be used from JS tooling, but isn't
  part of the compiled output unless explicitly used.
- Compiled JS files are fairly readable and have source map support for
  debugging
- Compiled JS files are linked to one shared NPM module `"cherry-cljs"` which
  contains `cljs.core.js`, `cljs.string`, etc.  such that libraries written in
  cherry can be compiled and hosted on NPM, while all sharing the same
  standard library and data structures. See [this
  tweet](https://twitter.com/borkdude/status/1549830159326404616) on how that
  looks.
- Output linked to older versions of cherry will work with newer
  versions of cherry: i.e. 'binary' compatibility.
- Light-weight and fast: heavy lifting such as optimizations are expected to be
  done by JS tooling
- No dependency on Google Closure: this project will use it for bootstrapping
  itself (by using the CLJS compiler), but users of this project won't use it for compilation
- Macro support
- REPL support
- Async/await support. See [this tweet](https://twitter.com/borkdude/status/1549843802604638209) for a demo.
- Native support for JS object destructuring: `[^:js {:keys [a b]} #js {:a 1 :b 2}]`
- Native support for JSX via `#jsx` reader tag. See [example](https://github.com/squint-cljs/cherry/blob/main/examples/jsx/pages/component.cljs).

Cherry may introduce new constructs such as `js-await` which won't be compatible
with current CLJS. Also it might not support all features that CLJS offers. As
such, using existing libraries from the CLJS ecosystem or compiling Cherry CLJS
code with the CLJS compiler may become challenging. However, some results of
this experiment may end up as improvements in the CLJS compiler if they turn out
to be of value.

See [slides](https://www.dropbox.com/s/955jgzy6hgpx67r/dcd2022-cljs-reimagined.pdf?dl=0) of a presentation given at Dutch Clojure Days 2022 about cherry and squint.

Depending on interest both from people working on this and the broader
community, the above goals may or may not be pursued. If you are interested in
maturing cherry, please submit
[issues](https://github.com/squint-cljs/cherry/issues) for bug reports or share
your thoughts on [Github
Discussions](https://github.com/squint-cljs/cherry/discussions).

Cherry started out as a fork of
[Scriptjure](https://github.com/arohner/scriptjure). Currently it's being
reworked to meet the above goals.

<!-- ## Funding -->

<!-- This project is developed with the following partners, either by funding time -->
<!-- and/or money: -->

<!-- - [Nextjournal](https://nextjournal.com/) -->
<!-- - The main author's [Github Sponsors](https://github.com/sponsors/borkdude) -->

## `cherry.edn`

In `cherry.edn` you can describe the following options:

- `:paths`: the source paths to search for files. At the moment, only `.cljc` and `.cljs` are supported.
- `:extension`: the preferred extension to output, which defaults to `.mjs`, but can be set to `.jsx` for React(-like) projects.
- `:copy-resources`: a set of keywords that represent file extensions of files
  that should be copied over from source paths. E.g. `:css`, `:json`. Strings
  may also be used which represent regexes which are processed through
  `re-find`.
- `:output-dir`: the directory where compiled files will be created,
  which defaults to the project root directory.
- `:deps`: a map of dependencies from the Clojure ecosystem, in the same format
  as `deps.edn`. Only `:git/url` (with `:git/sha`) and `:local/root` libraries
  are supported, no jars yet. Their source directories are resolved via the
  `clojure` CLI (`-Spath`) and added to `:paths`. Requires `clojure` on the
  `PATH`. Example:
  ```clojure
  {:paths ["src"]
   :deps {io.github.cjohansen/replicant {:git/sha "..."}}}
  ```

See [examples/browser-repl](examples/browser-repl) for an example project which
uses a `cherry.edn`, including a git dependency via `:deps`.

## Watch

Run `npx cherry watch` to watch the source directories described in
`cherry.edn` and they will be (re-)compiled whenever they change.

## Testing

Cherry ships a `clojure.test`-compatible testing library, requirable as
`cljs.test` or `clojure.test`:

```clojure
(ns example-test
  (:require [cljs.test :as t :refer [deftest is]]))

(deftest math-test
  (is (= 4 (+ 2 2))))

(t/run-tests 'example-test)
```

## nREPL

An (currently immature!) nREPL implementation can be used on Node.js with:

``` shell
npx cherry nrepl-server --port 1888
```

Please try it out and file issues so it can be improved.

## Browser REPL

The `cherry-cljs/vite.js` plugin provides a browser REPL: it hot-reloads
compiled cherry in the page and runs an nREPL server that your editor connects
to, evaluating forms in the live page. See
[doc/browser-repl.md](doc/browser-repl.md).

## Embed cherry in a CLJS/shadow app

See [embed.md](doc/embed.md).

## Development

``` shell
$ git clone git@github.com:squint-cljs/cherry.git
$ cd cherry
$ bb dev
```

## `defclass`

See [squint docs](https://github.com/squint-cljs/squint/blob/main/doc/defclass.md).

## `js-template`

See [squint docs](https://github.com/squint-cljs/squint/blob/main/doc/defclass.md).

License
=======
Cherry is licensed under the EPL, the same as Clojure core and [Scriptjure](https://github.com/arohner/scriptjure). See epl-v10.html in the root directory for more information.
