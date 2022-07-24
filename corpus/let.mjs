const log = console.log;
log("hello");
log((1 + 2 + 3));
let y, inc;
y = let x;
x = (function () {
 log("in do");
return 12;
})();
(function () {
 log("x + 1 =", (x + 1));
return (x + 13);
})();
inc = "inc";
log("y =", y, inc);

export { log }
