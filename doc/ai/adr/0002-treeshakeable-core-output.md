# ADR 0002: Re-massage the Closure output of cljs.core.js for esbuild tree-shaking

Status: idea, not started.

## Problem

`lib/cljs.core.js` is ~390KB minified and loads as a unit. A user bundle that
imports one function from `cherry-cljs/cljs.core.js` keeps all of it: esbuild
tree-shakes at the level of top-level statements and bindings, and the Closure
output is one long series of `$APP.xx = ...` property assignments on the
shared rename-root object, plus genuine top-level side effects (prototype
stamps, base-type tables, the sentinel init). Every statement looks
side-effectful to esbuild, so nothing is dropped. Squint's hand-written
`core.js` with per-function exports shakes cleanly, cherry's compiled core is
the structural opposite.

## Idea

Post-process the Closure output (the ADR 0001 pattern, applied at scale):
analyze `lib/cljs.core.js` and rewrite it into a form esbuild can shake.

1. `$APP.xx = ...` property assignments become top-level `const`/`let`
   bindings. Property dataflow through `$APP` is invisible to esbuild,
   references between top-level vars are exactly what it tracks. This undoes
   Closure's property collapse onto one object without undoing the renaming.
2. `/* @__PURE__ */` on call expressions whose result is only assigned
   (allocation of keywords, symbols, protocol fns, table objects).
3. `/* @__NO_SIDE_EFFECTS__ */` on function declarations that are pure, so
   call sites become droppable without per-site annotations.
4. Real side effects stay unannotated: prototype stamps, base-type tables,
   the sentinel init (ADR 0001 explains why that IIFE must never be dropped).

## Constraints

- `$APP` is shared across cherry's modules: `compiler.js`, `clojure.string.js`
  and the rest import cljs.core internals through it. Localizing props in
  cljs.core.js alone breaks them. Direction: rewrite all modules in one pass.
  A second standalone shadow build of cljs.core (no cross-module sharing, so
  Closure may emit plain vars itself) was considered and not chosen: it would
  add a second core artifact to the npm package.
- `package.json` `sideEffects: false` is not an option for cljs.core.js: the
  prototype stamps and protocol tables are load-time effects user code relies
  on.
- Wrong `@__PURE__` is a silently dropped side effect. Annotations must be
  derived from analysis, not name lists, and verified by running the full
  test suite against a shaken bundle.
- `bb publish` currently passes `--ignore-annotations` to esbuild for
  `cherry.umd.js`, which discards these annotations. Revisit when they become
  intentional.
- Output shape drifts with shadow/Closure versions. Like the sentinel patch,
  every transform needs hard assertions that fail the build instead of
  shipping a half-rewritten bundle.

## Investigation steps

1. Baseline: bundle `import { str } from 'cherry-cljs/cljs.core.js'` with
   esbuild, measure what survives (expected: everything).
2. Catalog the top-level statement classes in the release output:
   `$APP.x = <literal|function|call>`, prototype stamps, table entries, bare
   var declarations, IIFEs.
3. Prototype the props-to-vars rewrite plus annotations on a slice, measure
   the esbuild result, run the suite against it.
4. Decide packaging: in-place rewrite of `lib/` vs an extra shake-friendly
   artifact.
