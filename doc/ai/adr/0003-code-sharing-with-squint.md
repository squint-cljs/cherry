# ADR 0003: Share code with squint, fork only on dialect semantics

Status: accepted as direction. First application: port `watch`.

## Principle

Most code should be shared between squint and cherry. A fork is justified
only when the two dialects genuinely emit or run different things. Anything
else is incidental duplication and rots: cherry's `internal/fn.cljc` fork
missed the named-variadic munge fix for months, the CLI fork missed help,
validation, error reporting and completions until #194 re-copied 300 lines
by hand.

## Inventory

Shared today (via the squint git dep):

- `squint.compiler-common`: the emitter core, dialect-gated by `:target` env
- `squint.internal.node.utils`: config and path resolution, keyed by config
  file since #963
- `squint.defclass`, `squint.internal.test`, parts of
  `squint.internal.macros`

Legitimate forks (dialect semantics differ):

- `cherry.internal.fn`, `deftype`, `protocols`: cherry emits CLJS-style
  `cljs$core$IFn$_invoke$arity$N` and protocol props for the cljs.core
  runtime, squint emits its native scheme (`squint$lang$variadic`, Symbol
  slots). These cannot merge while the runtimes differ.
- `cherry.compiler` vs `squint.compiler`: the drivers differ where the
  dialects do, though both lean on compiler-common.

Incidental forks (duplication, candidates to merge):

- `cherry.internal.cli` vs `squint.internal.cli`: after #194 nearly
  identical. Differences are the prog name, the log prefix, the config file
  and the command set (squint adds watch, repl, socket-repl, nrepl-server).
- `cherry.compiler.node` vs `squint.compiler.node`: same file-compilation
  and macro-loading shape, different compiler entry points.
- `cherry.internal.destructure`, `loop`: near-verbatim copies, no dialect
  content.

## Mechanism

Extend the established patterns instead of inventing new ones: shared
namespaces live in the squint repo and take the dialect as data, like
compiler-common's `:target` env key and utils' config-file argument. For the
CLI that means a `squint.internal.cli-common` holding compile-files,
copy-file, watch, evaluate, the error helpers, arg validation and the
spec/table scaffolding, parameterized by a dialect map:

    {:prog "cherry" :log-prefix "[cherry]" :config-file "cherry.edn"
     :compile-file cherry.compiler.node/compile-file
     :resolve-ns ... :commands #{:run :compile :watch :eval}}

Each dialect's cli ns shrinks to that map plus dialect-only commands.
Same recipe later for compiler.node and for retiring the destructure and
loop forks.

## Porting watch

Port `watch` by doing the CLI extraction, not by copying the fn: move
squint's watch (chokidar via dynamic import, recompile on add/change,
copy-resources otherwise) into the shared ns and give cherry the command via
its dialect map. Cherry adds chokidar as an optional dependency like squint.

## Consequences

- New CLI features and fixes land once, in the shared ns. The squint repo
  becomes the upstream for CLI behavior, cherry PRs bump the pin.
- Cross-repo changes need two PRs (squint then cherry), as with
  compiler-common today. Accepted cost.
- The legitimate-forks list is the watch list: any change there should ask
  whether the dialect gap it encodes still exists.
