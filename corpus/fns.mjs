import { get, js_obj, prn, __destructure_map, keyword, arrayMap } from 'cherry-cljs/cljs.core.js'

const foo = function foo (p__1123) {
return (function () {
 return (function () {
 let map__1124, a, b;
map__1124 = p__1123;
map__1124 = __destructure_map(map__1124);
a = get(map__1124, keyword("a"));
b = get(map__1124, keyword("b"));
return (function () {
 return (a + b);
})();
})();
})();
};
prn(foo(arrayMap(keyword("a"), 1, keyword("b"), 2)));
const bar = function (p__1126) {
return (function () {
 return (function () {
 let map__1127, a, b;
map__1127 = p__1126;
map__1127 = __destructure_map(map__1127);
a = get(map__1127, keyword("a"));
b = get(map__1127, keyword("b"));
return (function () {
 return (a + b);
})();
})();
})();
};
prn(bar(arrayMap(keyword("a"), 1, keyword("b"), 2)));
const baz = function (p__1129) {
return (function () {
 return (function () {
 let map__1130, a, b;
map__1130 = p__1129;
map__1130 = __destructure_map(map__1130);
a = map__1130["a"];
b = map__1130["b"];
return (function () {
 return (a + b);
})();
})();
})();
};
prn(baz(js_obj("a", 1, "b", 2)));
