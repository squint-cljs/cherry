# Browser REPL

The cherry vite plugin (`cherry-cljs/vite.js`) hot-reloads your cherry code in
the browser and lets you eval into the live page over nREPL. The following
setup runs it via `npm run dev`.

The plugin is tested with vite v8.

For a working setup, see [`examples/browser-repl`](../examples/browser-repl).

## Setup

`cherry.edn`:

```edn
{:paths ["src"]
 :output-dir "js"
 :extension "js"
 :main plain}
```

`vite.config.js`:

```js
import { defineConfig } from 'vite';
import cherry from 'cherry-cljs/vite.js';

export default defineConfig({ plugins: [cherry()] });
```

`index.html` (the plugin injects the `:main` entry ns):

```html
<div id="plain"></div>
```

`package.json`:

```json
{
  "type": "module",
  "scripts": {
    "dev": "vite dev",
    "build": "vite build"
  }
}
```

`npm run dev` compiles `:paths`, serves the app, and starts the nREPL server
(port `1339`, written to `.nrepl-port`).

`npm run build` produces a normal optimized bundle with regular (non-REPL)
cherry output, with the dev-only REPL/HMR stripped.

## Hot reload

Saving a `.cljs`/`.cljc` file recompiles it and hot-swaps the module in the
page, without a page reload. To run code before and after such a reload, define
functions tagged with `:dev/before-load` and `:dev/after-load` metadata.

```clojure
(defn ^:dev/after-load re-render []
  (swap! store identity))
```

`^:dev/before-load` runs before the new code loads.

## React / Preact (JSX)

Set `:jsx-runtime` so cherry emits `jsx()`/`jsxs()` calls (importing the
framework's runtime) instead of raw `<tags>`. This is what makes `#jsx` work at
the REPL and in the browser - the output is plain JS, with no separate JSX
transform step.

```edn
{:paths ["src"]
 :output-dir "js"
 :extension "js"
 :main [plain preact replicant-app]
 :jsx-runtime {:import-source "preact"}} ; or "react"
```

The plugin uses the dev runtime (`<import-source>/jsx-dev-runtime`) under
`vite dev` and the production runtime (`<import-source>/jsx-runtime`) for
`vite build`.

See `examples/browser-repl/src/preact.cljs` for a working component.

## Replicant (no JSX)

[Replicant](https://github.com/cjohansen/replicant) renders plain hiccup
vectors (`[:div ...]`) built from real cljs data - cherry's native fit. No
`:jsx-runtime` and no `#jsx`, so it works at the REPL out of the box. See
`examples/browser-repl/src/replicant_app.cljs` and
[`examples/replicant`](../examples/replicant) for a full app.

## Options

| key | meaning |
|---|---|
| `:main` | entry ns whose compiled module the plugin injects as a `<script>` into `index.html`, booting the app (so the output path isn't hardcoded); symbol/string, or a vector for several |
| `:paths` | source dirs (default `["src"]`) |
| `:output-dir` | output dir (default `"js"`) |
| `:extension` | output extension (default `"js"`) |
| `:nrepl-port` | nREPL port (default `1339`) |
| `:target` | runtime target (only `browser`) |
| `:jsx-runtime` | `{:import-source "react"\|"preact"}` to emit jsx-runtime calls for JSX (see above) |

## Optional: pre-bundle deps for a smoother REPL

The first time you require a dependency vite hasn't seen, vite pre-bundles it
and reloads the page, which causes loss of REPL state. To avoid this, list the
deps you reach for at the REPL in vite's `optimizeDeps.include`:

```js
export default defineConfig({
  optimizeDeps: {
    include: ['preact', 'preact/hooks', 'preact/jsx-runtime', 'preact/jsx-dev-runtime'],
  },
  plugins: [cherry()],
});
```
