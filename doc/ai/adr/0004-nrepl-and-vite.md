# ADR 0004: Port nREPL and vite REPL support from squint

Status: accepted. Stage 1 (nREPL) in progress, stage 2 (vite) blocked on a
squint npm release.

## Problem

Squint has an nREPL server (`squint nrepl-server`, editors connect over
bencode TCP) and a vite plugin with a browser REPL riding vite's HMR
WebSocket. Cherry has neither. Per ADR 0003 the port should share the
implementation, not copy it.

## Analysis

The nREPL server (`squint.repl.nrepl-server`, ~560 lines) is mostly
dialect-neutral: bencode, middleware, ops (eval, lookup, complete,
load-file), the browser transport seam. The dialect touches are the compile
call (`compile-string*`, `:repl`/`:repl-return` emission comes from shared
compiler-common), REPL ns resolution (`resolve-ns-repl`), the config file
and value printing (`pr-str` of the dialect's values). Cherry's compiler
bundle and code it evals share one cljs.core module instance, so cherry's
own `pr-str` prints eval results correctly.

The vite plugin (`vite.js`, ~420 lines of plain JS in the npm package root)
compiles via the package's node API, runs the nREPL server with a browser
transport, and injects a browser client that imports the dialect's core
module. It is npm-level code: cherry can only share it by importing from a
released `squint-cljs` package.

## Decision

Stage 1, nREPL (git-dep flow as usual):

- `squint.repl.print-common` and `squint.repl.nrepl-server-common` hold the
  implementation, parameterized by a dialect map
  (`:compile-string*`, `:resolve-ns-repl`, `:config-file`, `:pr-str-repl`),
  same recipe as compiler-common's `:target` and cli-common's dialect map.
- `squint.repl.nrepl-server` and `cherry.repl.nrepl-server` are thin
  wrappers supplying their dialect. The shadow `:node.nrepl_server` module
  and its JS exports (`startServer`, `handleBrowserMessage`, `evalString`)
  keep their names so squint's vite.js keeps working.
- The `nrepl-server` CLI command moves into cli-common, both dialects list
  it.
- Cherry verification: integration test driving `cherry nrepl-server` over
  bencode from babashka (bb bundles bencode).

Stage 2, vite: refactor `vite.js` into an exported factory taking a dialect
adapter (compile/config fns, nREPL imports, core module specifier, config
file name), squint's default export stays. Cherry ships a thin `vite.js`
importing the factory from `squint-cljs`. Blocked on releasing squint with
the factory and bumping cherry's `squint-cljs` npm dependency (pinned to
0.9.174 today).

## Consequences

- Editors get a cherry nREPL after stage 1, browser REPL after stage 2.
- The nREPL protocol surface stays owned by squint; cherry bumps the pin.
- Cherry's npm package grows a `lib/node.nrepl_server.js` module.
