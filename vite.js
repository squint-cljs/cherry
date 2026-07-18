// Cherry's vite plugin: squint's shared browser-REPL plugin with cherry's
// compiler and nREPL server. See squint-cljs/vite-common.js for the design.

import { compileFile, readConfig, depsPaths } from './lib/compiler.node.js';
import {
  startServer,
  handleBrowserMessage,
  evalString,
} from './lib/node.nrepl_server.js';
import { makeVitePlugin } from 'squint-cljs/vite-common.js';

export default makeVitePlugin({
  name: 'cherry',
  coreImport: 'cherry-cljs/cljs.core.js',
  compileFile,
  readConfig,
  depsPaths,
  startServer,
  handleBrowserMessage,
  evalString,
});
