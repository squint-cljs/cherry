// Simulates a dev-mode ClojureScript host runtime loaded before cherry
// (issue #190): the host publishes cljs.core.PROTOCOL_SENTINEL and stamps
// Date.prototype, then cherry's cljs.core loads into the same realm.

import assert from 'node:assert/strict';

const HOST_SENTINEL = {};
globalThis.cljs = { core: { PROTOCOL_SENTINEL: HOST_SENTINEL } };

// what upstream cljs.core does at load
Date.prototype.cljs$core$IEquiv$ = HOST_SENTINEL;
Date.prototype.cljs$core$IEquiv$_equiv$arity$2 = function (_, other) {
  return other instanceof Date && this.valueOf() === other.valueOf();
};
Date.prototype.cljs$core$IComparable$ = HOST_SENTINEL;
Date.prototype.cljs$core$IComparable$_compare$arity$2 = function (_, other) {
  if (other instanceof Date) {
    const a = this.valueOf(), b = other.valueOf();
    return a < b ? -1 : a > b ? 1 : 0;
  }
  throw new Error('Cannot compare');
};

// host-side dispatch, as compiled by the host's cljs
function hostSatisfiesIComparable(x) {
  return HOST_SENTINEL === x.cljs$core$IComparable$;
}

const core = await import('../../lib/cljs.core.js');

// cherry must not clobber the host's markers with a foreign token
assert.ok(hostSatisfiesIComparable(new Date()),
  'host: Date no longer satisfies IComparable after cherry loaded');

// cherry must have adopted the shared sentinel
assert.strictEqual(core.PROTOCOL_SENTINEL, HOST_SENTINEL,
  'cherry: PROTOCOL_SENTINEL not shared with host');

// cherry-side dispatch on natives must still work after adoption
assert.strictEqual(core.compare(new Date(1000), new Date(2000)), -1,
  'cherry: compare on Dates broken');

// cherry-side dispatch on its own types must still work after adoption
// (their markers were stamped with the old token during cljs.core load)
const v = core.vector(1, 2, 3);
assert.strictEqual(core.first(v), 1, 'cherry: first on vector broken');
assert.strictEqual(core.get(core.hash_map('a', 1), 'a'), 1,
  'cherry: hash-map lookup broken');
assert.strictEqual(core.compare(core.keyword('a'), core.keyword('b')), -1,
  'cherry: compare on keywords broken');

console.log('host-first: OK');
