# treeshake: production transform for ADR 0002

Babel-based successor to the spike in `spike/`. AST-only (no textual
substitution), scope-aware self-reference renaming, and hard assertions on
every shape assumption: an unrecognized statement class, a computed `$APP`
access, a name collision or an unresolvable prop fails the build instead of
shipping a half-rewritten bundle.

Emits the facade packaging: `out/cljs.core.js` re-exports the public API
from `out/internal/cljs.core.js`; lib modules import core internals by name.

    npm install
    node treeshake.mjs --core testdata/cljs.core.orig.js \
      --lib clojure.string=testdata/clojure.string.orig.js \
      --lib clojure.walk=testdata/clojure.walk.orig.js --out out
    node dce_test.mjs

The DCE tests prove: a negative control (the untransformed core must NOT
shake), byte budgets (floor 2.0 KB, `import { not }` 2.1 KB, `str` 136 KB),
an unchanged public export surface, zero facade overhead, and differential
runtime equivalence (identical stdout for compiled cherry programs on
baseline vs shaken cores).

Not covered yet: the compiler and nREPL modules (same mechanism as the lib
modules), build integration (`bb` task ordering with the sentinel patch),
and CI wiring.
