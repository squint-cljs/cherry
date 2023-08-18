[Cherry](https://github.com/squint-cljs/cherry): Experimental ClojureScript to ES6 module compiler

## 0.1.6 (2023-08-18)

- Add `clojure.string` and `clojure.walk`

## 0.1.5 (2023-08-15)

- Support `defclass` and `js-template`, same as squint. See [squint docs](https://github.com/squint-cljs/squint/blob/main/doc/defclass.md).

## 0.0.4 (2023-06-03)

- [#101](https://github.com/squint-cljs/cherry/issues/101): include `lib/cherry.umd.js` fallback for Firefox web worker

## 0.0.3 (2023-05-13)

- Adjust core package in standard import

## 0.0.2 (2023-05-13)

- [#93](https://github.com/squint-cljs/cherry/issues/93): optimize `if` + `zero?`
- [#98](https://github.com/squint-cljs/cherry/issues/98): fix `$default` import

## 0.0.1

- Support `cherry.embed` namespace to embed cherry into a CLJS/shadow app. See [docs](doc/embed.md).
- Many other small fixes

## 0.0.0-alpha.60

- [#78](https://github.com/squint-cljs/cherry/issues/78): fix macro call with more than 20 arguments
- [#79](https://github.com/squint-cljs/cherry/issues/79): fix issue with advanced compilation and `_EQ_` symbol

## 0.0.0-alpha.59

- [#78](https://github.com/squint-cljs/cherry/issues/78): fix macro call with more than 20 arguments
- [#79](https://github.com/squint-cljs/cherry/issues/79): fix (workaround) issue with advanced compilation and `_EQ_` symbol

## 0.0.0-alpha.58

- [#71](https://github.com/squint-cljs/cherry/issues/71): support alias with dashes
- [#77](https://github.com/squint-cljs/cherry/issues/77): support async/await in variadic function
s
- [#67](https://github.com/squint-cljs/cherry/issues/67): support namespaced components in JSX

## 0.0.0-alpha.57

- Fix rendering of number attributes in JSX
