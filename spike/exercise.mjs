import * as cherry_core from 'cherry-cljs/cljs.core.js';
cherry_core.prn.call(null, cherry_core.assoc.call(null, cherry_core.array_map(cherry_core.keyword("a"), 1), cherry_core.keyword("b"), 2));
cherry_core.prn.call(null, cherry_core.mapv.call(null, cherry_core.inc, cherry_core.vector(1, 2, 3)));
cherry_core.prn.call(null, cherry_core.reduce.call(null, cherry_core._PLUS_, cherry_core.range.call(null, 10)));
cherry_core.prn.call(null, `x1${cherry_core.keyword("kw")??''}${cherry_core.symbol.call(null, "sym")??''}${cherry_core.vector(1, 2)??''}${cherry_core.array_map(cherry_core.keyword("a"), 1)??''}${cherry_core.hash_set(3)??''}`);
cherry_core.prn.call(null, cherry_core.into.call(null, cherry_core.array_map(), cherry_core.map.call(null, (function (x) {
return cherry_core.vector(x, (x * x));

}), cherry_core.range.call(null, 3))));
var a = cherry_core.atom.call(null, cherry_core.array_map(cherry_core.keyword("n"), 0));
cherry_core.swap_BANG_.call(null, a, cherry_core.update, cherry_core.keyword("n"), cherry_core.inc);
cherry_core.prn.call(null, cherry_core.deref.call(null, a));
var IThing = function(){};
let IThing$thing$dyn1 = (function (_) {
const x__22731__auto__1 = (((_ == null)) ? (null) : (_));
const m__22732__auto__2 = (thing[cherry_core.goog_typeOf.call(null, x__22731__auto__1)]);
if (cherry_core.truth_.call(null, cherry_core.not.call(null, (m__22732__auto__2 == null)))) {
return m__22732__auto__2.call(null, _)} else {
const m__22730__auto__3 = (thing["_"]);
if (cherry_core.truth_.call(null, cherry_core.not.call(null, (m__22730__auto__3 == null)))) {
return m__22730__auto__3.call(null, _)} else {
throw cherry_core.missing_protocol.call(null, "IThing.thing", _)};
};

});
var thing = function (_) {
if (cherry_core.truth_.call(null, (() => {
const and__23981__auto__4 = cherry_core.not.call(null, (_ == null));
if (cherry_core.truth_.call(null, and__23981__auto__4)) {
return cherry_core.not.call(null, (_.IThing$thing$arity$1 == null))} else {
return and__23981__auto__4};

})())) {
return _.IThing$thing$arity$1(_)} else {
return IThing$thing$dyn1.call(null, _)};

};
var T = function (x) {
this.x = x;

};
T.prototype.IThing$ = cherry_core.PROTOCOL_SENTINEL;
T.prototype.IThing$thing$arity$1 = (function (_) {
const self__ = this;
const _1 = this;
return self__.x;

});
T.getBasis = (function () {
return cherry_core.vector(cherry_core.symbol.call(null, "x"));

});
T.cljs$lang$type = true;
T.cljs$lang$ctorStr = "exercise/T";
T.cljs$lang$ctorPrWriter = (function (this__23468__auto__, writer__23469__auto__, opt__23470__auto__) {
return cherry_core._write.call(null, writer__23469__auto__, "exercise/T");

});
var __GT_T = function (x) {
return (new T(x));

};
T;
cherry_core.prn.call(null, thing.call(null, __GT_T.call(null, 42)));
var area = (new cherry_core.MultiFn(cherry_core.symbol.call(null, "area"), cherry_core.keyword("shape"), cherry_core.keyword("default"), cherry_core.get_global_hierarchy.call(null), cherry_core.atom.call(null, cherry_core.array_map()), cherry_core.atom.call(null, cherry_core.array_map()), cherry_core.atom.call(null, cherry_core.array_map()), cherry_core.atom.call(null, null)));
cherry_core._add_method.call(null, area, cherry_core.keyword("square"), (function (p__2) {
const map__31 = p__2;
const map__32 = cherry_core.__destructure_map.call(null, map__31);
const side3 = cherry_core.get.call(null, map__32, cherry_core.keyword("side"));
return (side3 * side3);

}));
cherry_core.prn.call(null, area.call(null, cherry_core.array_map(cherry_core.keyword("shape"), cherry_core.keyword("square"), cherry_core.keyword("side"), 3)));
cherry_core.prn.call(null, cherry_core.sort.call(null, cherry_core.shuffle.call(null, cherry_core.vector(3, 1, 2))));
cherry_core.prn.call(null, cherry_core.frequencies.call(null, "abracadabra"));
cherry_core.prn.call(null, cherry_core.partition.call(null, 2, cherry_core.interleave.call(null, cherry_core.vector(cherry_core.keyword("a"), cherry_core.keyword("b")), cherry_core.vector(1, 2))));
cherry_core.prn.call(null, cherry_core._EQ_.call(null, cherry_core.list.call(null, 1, 2), cherry_core.seq.call(null, cherry_core.vector(1, 2))));
cherry_core.prn.call(null, cherry_core.keyword.call(null, "k"), cherry_core.symbol.call(null, "s"));
cherry_core.prn.call(null, cherry_core.pr_str.call(null, cherry_core.array_map(cherry_core.keyword("nested"), cherry_core.vector(1, cherry_core.hash_set(cherry_core.keyword("a")), cherry_core.list.call(null, "x")))));
cherry_core.println.call(null, "println-works");
let _STAR_print_length_STAR__orig_val__46 = cherry_core._STAR_print_length_STAR_.val;
let _STAR_print_length_STAR__temp_val__57 = 2;
cherry_core._STAR_print_length_STAR_.val = _STAR_print_length_STAR__temp_val__57;
try{
cherry_core.prn.call(null, cherry_core.vector(1, 2, 3, 4))}
finally{
cherry_core._STAR_print_length_STAR_.val = _STAR_print_length_STAR__orig_val__46}
;
cherry_core.prn.call(null, (new cherry_core.LazySeq(null, (function () {
return cherry_core.cons.call(null, 1, (new cherry_core.LazySeq(null, (function () {
return null;

}), null, null)));

}), null, null)));
cherry_core.prn.call(null, cherry_core.take.call(null, 3, cherry_core.iterate.call(null, cherry_core.inc, 0)));
cherry_core.prn.call(null, cherry_core.group_by.call(null, cherry_core.odd_QMARK_, cherry_core.range.call(null, 6)));
cherry_core.prn.call(null, cherry_core.get_in.call(null, cherry_core.array_map(cherry_core.keyword("a"), cherry_core.array_map(cherry_core.keyword("b"), cherry_core.array_map(cherry_core.keyword("c"), 1))), cherry_core.vector(cherry_core.keyword("a"), cherry_core.keyword("b"), cherry_core.keyword("c"))));

export { a, IThing, thing, __GT_T, area }
