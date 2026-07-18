# browser REPL

Three counters rendered three ways (plain `#html`, preact `#jsx`, replicant
hiccup), compiled by cherry and served by vite with the cherry plugin. The
plugin runs an nREPL server that evaluates in the browser.

Run:

    npm install
    npm run dev

Connect your editor to the nREPL port (1339 by default, `.nrepl-port`).
