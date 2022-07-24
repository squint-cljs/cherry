import { __destructure_map, get, keyword, prn, arrayMap } from 'cherry-cljs/cljs.core.js'
const foo = function foo (p__1) {
let map__2, a, b;
map__2 = p__1;
map__2 = __destructure_map(map__2);
a = get(map__2, keyword("a"));
b = get(map__2, keyword("b"));
return (a + b);
};
prn(foo(arrayMap(keyword("a"), 1, keyword("b"), 2)));
const bar = function (p__3) {
let map__4, a, b;
map__4 = p__3;
map__4 = __destructure_map(map__4);
a = get(map__4, keyword("a"));
b = get(map__4, keyword("b"));
return (a + b);
};
prn(bar(arrayMap(keyword("a"), 1, keyword("b"), 2)));
const baz = function (p__5) {
let map__6, a, b;
map__6 = p__5;
map__6 = __destructure_map(map__6);
a = map__6["a"];
b = map__6["b"];
return (a + b);
};
prn(baz({ "a": 1, "b": 2 }));

export { foo, bar, baz }
