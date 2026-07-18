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

## Spike results (2026-07-18)

Prototyped in `0002-spike-rewrite.mjs` (acorn-based, run against the release
`lib/cljs.core.js`, output verified by running cherry-compiled code covering
collections, seqs, atoms, protocols, deftype, defmulti, print and binding).
Companions: `0002-spike-analyze.mjs` profiles which statements anchor a
bundle, `0002-spike-exercise.cljs` is the runtime acceptance harness,
`0002-spike-rewrite-lib.mjs` is the cross-module extension.

Measurements (esbuild --bundle --minify):

| bundle | before | after | gzip after |
|---|---|---|---|
| bare `import 'cljs.core.js'` (side-effect floor) | 364 KB | 2.0 KB | 0.9 KB |
| `import { str }` | 364 KB | 133 KB | 24 KB |
| app exercising most of core | ~370 KB | 198 KB | 35 KB |

The transform, in order:

1. Singly-assigned `$APP.p = rhs` (613 of 614 props) become `var p = rhs`;
   all references follow. No name collisions with Closure's plain vars, no
   computed `$APP[..]` access. The one multi-assigned prop is `g`, Closure's
   prototype-stamp cursor.
2. Contiguous arity installs (`p.h = ..`) fold into the def as
   `var p = /* @__PURE__ */ (function(){var $self=..;..;return $self})()`.
3. Type constructors merge with their `$APP.g = T.prototype` stamp segment
   into one PURE initializer (stamps move up to the def), gated on every
   eager identifier in the moved code being declared before the def.
   Native targets (Date) stay as real side effects.
4. Closure's hoisted `var ha,oa,..;` names with exactly one top-level
   assignment become declaration-at-assignment.
5. Multi-declarator `var a=..,b=..;` statements split, then a fixpoint pass
   folds every remaining `T.x = ..` / cursor segment into T's initializer.
   The sentinel init moves to the top of the file (deps: globals only).
6. Remaining call/new initializers (dispatch-builder IIFEs, keyword/symbol
   singletons, the sentinel) get `@__PURE__`.

Findings:

- The floor collapsed from 364 KB to 2 KB only after step 5: three orphan
  arity installs on `obj-map` (hidden in a multi-declarator) anchored the
  entire graph. Anchor analysis (bundle a bare import, classify surviving
  statements) is the essential debugging loop; single statements can hold
  100+ KB alive through the protocol/seq web.
- `import { str }` at 133 KB shows the practical floor for collection-using
  code: equality, seq and print machinery are densely interconnected.
  Fn-level granularity delivers proportionate sizes, not miracles.
- Textual self-reference substitution must exclude `.name` property
  positions and must not use an alias that Closure also uses for locals
  (`f` shadowed as a loop counter produced `new f(..)` on a number - caught
  by the runtime exercise, not by parsing). A production transform should
  substitute via the AST, not regexes.
- Cross-module breakage confirmed: `clojure.string.js` and the rest import
  `$APP` and died immediately. The one-pass all-modules rewrite from the
  constraints section is mandatory; per-module named exports would replace
  the `$APP` carrier.
- `enable-console-print!` (`Ea()`) and the Date protocol stamps stay as
  load-time effects and cost ~nothing once nothing else anchors the graph.

Not done, needed for production: all-modules pass, AST-based emitter with the
ADR's hard drift assertions, full test suite + e2e against the shaken
artifacts, and packaging (in-place vs extra artifact), which also interacts
with `sideEffects: false` in package.json.

### Cross-module addendum (same day)

`rewrite-lib.mjs` extends the transform to `clojure.string.js` and
`clojure.walk.js`: the shaken core exports its 916 internal names, lib
modules turn their own `$APP` defs into vars and import core internals by
name, the stamp cursor goes module-local. Hard assertion: no `$APP`
reference survives.

The replicant tic-tac-toe example (esbuild, same app both sides):

| | minified | gzip |
|---|---|---|
| original core + libs | 439.5 KB | 80.7 KB |
| shaken | 281.7 KB | 52.7 KB |

Verified by serving the shaken bundle and playing the game in Chrome:
renders, alternates turns, zero console errors.

### Pseudo-names experiment (same day)

Rebuilt with `:compiler-options {:pseudo-names true}` (raw core: 416 KB ->
1.46 MB, same advanced optimizations) and reran the transform unchanged
except for two detector generalizations it forced - both reproducibility
fixes, now permanent:

- the prototype cursor is derived (multi-assigned prop whose assignments are
  all `X.prototype`): `g` in short-name builds,
  `$JSCompiler_prototypeAlias$$` under pseudo-names
- the sentinel init is detected by the `.PROTOCOL_SENTINEL` property access
  with globals-only eager deps; the substring alone also matches type inits
  that merely reference the sentinel var

With those, the pipeline reproduced on entirely different names (557/558
props single-assigned, 89 folds, no blockers) and the runtime exercise
passes. This empirically answers reproducibility: everything is re-derived
per build; only output-shape drift can break it, and that fails loudly.

Sizes, transform + esbuild --minify on the pseudo build vs short names:

| bundle | short names | pseudo + esbuild minify |
|---|---|---|
| floor | 2.0 KB / 0.9 gz | 2.3 KB / 1.0 gz |
| `import { str }` | 133 KB / 24.0 gz | 166 KB / 25.4 gz |
| exercise app | 198 KB / 34.7 gz | 254 KB / 37.5 gz |

The delta is property names: esbuild cannot safely re-mangle properties, so
pseudo-named protocol/arity props (`cljs$core$ISeqable$_seq$arity$1` style)
ship long - +25% minified, +7% gzipped. Verdict: keep releases on Closure's
short names (the transform no longer cares), use pseudo-names as a debug
flag for developing the transform. A shipped pseudo-names pipeline would
also need every module minified, including the compiler; that works but
buys nothing over short names.
