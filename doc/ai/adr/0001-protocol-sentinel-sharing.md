# ADR 0001: Share PROTOCOL_SENTINEL with coexisting CLJS runtimes (issue #190)

Status: implemented (branch `sentinel-handshake`).

## Problem

CLJS dispatches protocols on native types via marker properties on shared
prototypes: `Date.prototype.cljs$core$IComparable$ = PROTOCOL_SENTINEL`,
checked by identity (`PROTOCOL_SENTINEL === x.cljs$core$IComparable$`).
There is one `Date.prototype` per JS realm but one sentinel per runtime, so
whichever runtime loads last overwrites the marker and the other concludes
`Date` no longer implements the protocol. `(compare date1 date2)` then
throws `Cannot compare`.

Upstream guards this: `PROTOCOL_SENTINEL` is defonce'd, so a runtime reuses
`cljs.core.PROTOCOL_SENTINEL` from the global when an earlier runtime
published one. In cherry's npm artifact (`shadow-cljs release`, advanced
optimizations) Closure collapses `cljs.core.PROTOCOL_SENTINEL` into a
renamed var (`$APP.cd`) and folds the guard into an unconditional
`$APP.cd={}`. Cherry therefore always mints a private sentinel, clobbers
coexisting runtimes and publishes nothing. The marker property names
themselves survive (they are listed in `externs/cherry.txt`), so only the
sentinel identity mismatches.

## Decision

Patch the single init site in the built `lib/cljs.core.js` after
`shadow-cljs release` (`patch-protocol-sentinel` in `bb/tasks.clj`):

1. Find the renamed sentinel via the ESM export binding
   `PROTOCOL_SENTINEL=$APP.cd`. The export name is public npm API
   (`resources/cherry/cljs.core.edn`) and can never be renamed, so it is a
   stable anchor. The renamed name itself differs per build.
2. Replace the one occurrence of `$APP.cd={}` with an expression that
   resolves the token in three steps: reuse
   `globalThis.cljs.core.PROTOCOL_SENTINEL` when present, else adopt the
   value of `Date.prototype.cljs$core$IEquiv$` when a foreign runtime with
   literal marker names already stamped it (e.g. an unpatched cherry), else
   mint one. Whatever it resolves is published at the global path.

The patched init runs before any of the ~63 stamp sites in cljs.core, so
every marker is born with the shared token and nothing needs restamping.
Two intentional properties of the injected expression: it always creates
`globalThis.cljs.core` (one key, the sentinel), since publishing eagerly is
what makes cherry-loads-first orderings work and later goog-based runtimes
merge into the existing object, and it carries no `@__PURE__` annotation,
since dropping it in downstream DCE would silently drop the publish side
effect.
Cost: +121 bytes. The patch throws when the export is missing or the init
site count is not exactly one, so a shadow or Closure output change fails
the build instead of shipping unpatched. Two-realm node tests
(`test-resources/sentinel/*.mjs`, host-first and cherry-first) run in
`bb test`.

## Scope

The handshake works with runtimes that participate in the global sentinel
convention: dev builds (`:none`), `:simple` builds, self-hosted runtimes
and other patched cherry instances. Advanced hosts split by marker naming:
with renamed markers (the vanilla case) the property names are disjoint and
there is no conflict to fix, with literal marker names (an unpatched cherry
or a cherry-like externs setup) the marker sniff adopts their token when
they load first. The one remaining broken ordering is such a host loading
last: it overwrites unconditionally and nothing cherry ships can prevent
that. Upstream is strictly worse off in all of these cases, this exceeds
parity. `cherry.embed` is unaffected: embedded
code compiles against the host's cljs.core, one runtime, one sentinel.
Base types (`nil`, `string`, `number`, `boolean`, `array`, `function`,
`object`, `default`) are also unaffected: they dispatch via lookup tables
on each runtime's own protocol and method fn objects, not via the sentinel,
and write nothing shared.

## Decision matrix

|                                | init patch (chosen) | handshake ns   | externs | own marker names | do nothing |
|--------------------------------|---------------------|----------------|---------|------------------|------------|
| fixes #190 (participating host)| good                | good           | bad     | good             | bad        |
| unpatched-cherry coexistence   | ok (sniff, when it loads first) | ok (sniff possible) | bad | good | bad   |
| patched-cherry coexistence     | good                | good           | bad     | bad (needs token)| bad        |
| bundle size                    | good (+121 B)       | bad (+11.8 KB) | good    | good             | good       |
| implementation simplicity      | good (build step)   | bad (macro + restamp) | good | bad (mass rename) | good  |
| correct by construction        | good (init before stamps) | bad (post-load restamp) | -  | good  | -   |
| native impl interop            | good                | good           | -       | bad (forfeited)  | bad        |
| embed unaffected               | good                | good           | -       | bad (mode split) | good       |
| fails loudly on toolchain drift| good (build throws) | bad (silent miss) | -    | bad (silent)     | -          |
| proven                         | good (implemented, tested) | ok (built, measured) | bad (refuted) | bad (not built) | - |

## Alternatives rejected

### Compiled handshake namespace (SCI-style)

A cljc namespace in the `:cljs.core` module: a macro derives the list of
stamped type prototypes from cljs.core analyzer data (types with non
fast-path protocols), at load it adopts a host sentinel when present,
re-seats markers by value identity walk over those prototypes plus natives,
then `set!`s the var, or publishes otherwise. Implemented and passing, but
referencing 78 type prototypes defeats DCE: +11.8KB minified (+3%).
Rejected on size against 121 bytes for the same behavior.

### Externs

Declaring `cljs.core.PROTOCOL_SENTINEL` in a JS externs file or in
`externs/cherry.txt` has no effect: CollapseProperties flattens the
namespace before property renaming applies, and the folded defonce guard
never comes back. There is no config-only fix.

### Cherry-specific marker property names

Using cherry-owned marker names would isolate cherry from all hosts,
including advanced-compiled ones the handshake cannot reach. Rejected for
now: it requires mass-renaming markers in the compiled stock cljs.core and
a standalone/embed split in the emitter (embed must keep host names), it
does not fix cherry-vs-cherry coexistence (still needs a shared token), and
it forfeits the native-impl interop the shared convention intentionally
provides. Method slot collisions are harmless either way, impls are
behaviorally equivalent.

## Side discovery

`externs/cherry.txt` is incomplete: `IInst` and `IIterable` markers get
Closure-renamed (`.Sd`, `.se`) in the release bundle while cherry's emitter
writes literal names, so a user `deftype` implementing them is likely
broken against the release bundle. Separate issue, fix by completing or
generating that list.
