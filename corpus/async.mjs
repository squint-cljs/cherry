const status = async function () {
let resp, status;
resp = await(fetch("https://clojure.org"));
status = await(resp["status"]);
return status;
};
console.log("status:", await(status()));

export { status }
