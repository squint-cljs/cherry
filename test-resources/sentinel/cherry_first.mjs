// Cherry loads first into an empty realm: it must publish its sentinel at
// the conventional global path so a later dev-mode cljs host (defonce
// guard) and any later cherry instance reuse it.

import assert from 'node:assert/strict';

const core = await import('../../lib/cljs.core.js');

assert.ok(globalThis.cljs && globalThis.cljs.core,
  'cherry did not create the cljs.core global path');
assert.strictEqual(globalThis.cljs.core.PROTOCOL_SENTINEL, core.PROTOCOL_SENTINEL,
  'cherry did not publish its sentinel');

// sanity: cherry itself works
assert.strictEqual(core.compare(new Date(1000), new Date(2000)), -1);
assert.strictEqual(core.first(core.vector(1, 2, 3)), 1);

console.log('cherry-first: OK');
