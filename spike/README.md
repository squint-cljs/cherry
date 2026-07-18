# ADR 0002 spike: tree-shakeable core artifacts

Working output of the tree-shaking spike. See
`doc/ai/adr/0002-treeshakeable-core-output.md` on main for the full record.

## Contents

- `rewrite.mjs`: the seven-pass transform, `lib/cljs.core.js` -> shakeable.
  Name-agnostic: reruns on any build of the same shape.
- `rewrite-lib.mjs`: cross-module extension for `clojure.string.js` and
  `clojure.walk.js`.
- `analyze.mjs`: classifies which statements anchor a bundle. Run this first
  when a bundle is bigger than expected.
- `cljs.core.shaken.shortnames.js`: the transformed core (short-name build).
- `core_impl.js` / `core_facade.js`: the facade packaging. `core_impl.js`
  exports internals and public names, `core_facade.js` re-exports the public
  API only.
- `clojure.string.shaken.js`, `clojure.walk.shaken.js`: lib modules rewritten
  to named imports of core internals.
- `exercise.mjs`, `tiny.mjs`: cherry-compiled programs used as runtime
  acceptance tests (`doc/ai/adr/0002-spike-exercise.cljs` is the source of
  the first).

## Reproduce

The transforms read `../lib/*.js`, so run them next to a built `lib/`:

    npm install acorn
    node rewrite.mjs        # writes cljs.core.shaken.js
    node rewrite-lib.mjs    # writes the lib modules + internal exports

## Measure

    echo "import { str } from './core_facade.js'; console.log(str('a', 1))" > e.mjs
    npx esbuild e.mjs --bundle --minify --format=esm --outfile=out.js
    gzip -c out.js | wc -c

Results against these artifacts (minified / gzipped):

| bundle | before | after |
|---|---|---|
| bare import (side-effect floor) | 364 KB / 62 KB | 2.0 KB / 0.9 KB |
| assoc/get program (`tiny.mjs`) | 373 KB / 62.5 KB | 137 KB / 24.3 KB |
| full-core program (`exercise.mjs`) | ~370 KB | 198 KB / 34.7 KB |
| replicant tic-tac-toe | 439 KB / 80.7 KB | 282 KB / 52.7 KB |

## Run the acceptance tests

Point `cherry-cljs/cljs.core.js` at `core_facade.js` (npm alias or esbuild
`--alias`) and run:

    node exercise.mjs
    node tiny.mjs
