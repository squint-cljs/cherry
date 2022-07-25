import { truth_, not, keyword_QMARK_, name, vector, keyword } from 'cherry-cljs/cljs.core.js'
import * as confetti from 'https://cdn.skypack.dev/canvas-confetti';
import * as react from 'https://cdn.skypack.dev/react';
import { useEffect } from 'https://cdn.skypack.dev/react';
import * as rdom from 'https://cdn.skypack.dev/react-dom';
var $ = function (elt, props, children) {
let children1 = (not(children)) ? (props) : (children);
let elt2 = (keyword_QMARK_(elt)) ? (name(elt)) : (elt);
return react.createElement(elt2, props, children1);
};
var App = function () {
useEffect(function () {
return confetti["default"]();
}, vector());
return $(keyword("div"), $(keyword("code"), "(+ 1 2 3)"));
};
rdom.render(react.createElement(App), document.getElementById("app"));

export { $, App }
