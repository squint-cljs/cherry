import { List, Map, isImmutable } from 'immutable';

const map = (f, coll) => {
  return coll.map(f);
};

const filter = (f, coll) => {
  return coll.filter(f);
};

const vector = (...x) => {
  return List(x);
};

const prn = (...x) => {
  console.log(...x);
};

const str = (x) => {
  return x.toString();
};

const assoc = (coll, k, v) => {
  return coll.set(k, v);
};

const arrayMap = Map;

const keyword = (x) => x;

const keyword_QMARK_ = (x) => {
  return x instanceof String;
};

const nth = (coll, idx) => {
  if (coll instanceof Array) {
    return coll[idx];
  }
  if (isImmutable(coll)) {
    return coll.get(idx);
  }
  return null;
};

const _nth = nth;

const IndexedSeq = List;

const alength = (x) => {
  return x.length;
};

const array = (...x) => {
  return [...x];
};

const concat = (x, ...xs) => {
  return x.concat(...xs);
};

const first = (coll) => {
  return coll.first();
};

const list = (...x) => List(x);

const map_QMARK_ = (coll) => {
  return coll instanceof Map;
};

const name = (x) => x;

const seq = (coll) => {
  if (null === coll) return null;
  if (coll.size === 0)
    return null;
  else
    return coll.toSeq();
};

const next = (coll) => {
  return seq(coll.rest());
};

const sequence = coll => {
  let r = seq(coll);
  if ( null === r) {
    return List([]).toSeq();
  } else {
    return r;
  }
};

const symbol = (x) => x;

const truth_ = (x) => x;

const inc = (x) => x + 1;

const chunk_first = (x) => {
  return x;
};

const chunk_rest = (x) => {
  return x;
};

const chunked_seq_QMARK_ = (x) => false;

const count = (x) => x.size();

const unchecked_inc = inc;

const subs = (s, start, end) => {
  return s.substring(start, end);
};

export { map, filter, vector, prn, str, arrayMap, assoc, keyword, nth, keyword_QMARK_
         , IndexedSeq, alength, array, concat, first, list, map_QMARK_, name, next, seq
         , sequence, symbol, truth_, inc, _nth, chunk_first, chunk_rest, chunked_seq_QMARK_
         , count, unchecked_inc, subs }
