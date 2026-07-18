import * as cherry_core from 'cherry-cljs/cljs.core.js';
var m = cherry_core.assoc.call(null, cherry_core.array_map(cherry_core.keyword("x"), 0), cherry_core.keyword("a"), 1, cherry_core.keyword("b"), 2);
console.log(cherry_core.get.call(null, m, cherry_core.keyword("a")), cherry_core.get.call(null, m, cherry_core.keyword("b")), cherry_core.get.call(null, m, cherry_core.keyword("missing"), 42));

export { m }
