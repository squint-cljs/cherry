// Simulates an advanced-compiled runtime with literal marker names, e.g.
// an unpatched cherry, loaded before this cherry: it stamps Date.prototype
// but publishes no global sentinel. Cherry must adopt the token found in
// the marker and publish it.

import assert from 'node:assert/strict';

const HOST_SENTINEL = {};

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

function hostSatisfiesIComparable(x) {
  return HOST_SENTINEL === x.cljs$core$IComparable$;
}

const core = await import('../../lib/cljs.core.js');

assert.strictEqual(core.PROTOCOL_SENTINEL, HOST_SENTINEL,
  'cherry did not adopt the marker token');
assert.ok(hostSatisfiesIComparable(new Date()),
  'host: Date no longer satisfies IComparable after cherry loaded');
assert.strictEqual(globalThis.cljs.core.PROTOCOL_SENTINEL, HOST_SENTINEL,
  'cherry did not publish the adopted token');
assert.strictEqual(core.compare(new Date(1000), new Date(2000)), -1,
  'cherry: compare on Dates broken');
assert.strictEqual(core.first(core.vector(1, 2, 3)), 1,
  'cherry: first on vector broken');

console.log('advanced-host-first: OK');
