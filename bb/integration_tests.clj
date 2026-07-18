(ns integration-tests
  (:require [babashka.fs :as fs]
            [babashka.process :as p :refer [sh shell]]
            [bencode.core :as bencode]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is]]))

(defn- bytes->str [x]
  (if (bytes? x) (String. ^bytes x) x))

(defn- nrepl-exchange
  "Send one nREPL request and read responses until a done status. Returns the
  merged response map with string values."
  [in out msg]
  (bencode/write-bencode out msg)
  (loop [acc {}]
    (let [resp (update-vals (bencode/read-bencode in) bytes->str)
          acc (merge acc resp)]
      (if (some #(= "done" (bytes->str %)) (get resp "status"))
        acc
        (recur acc)))))

(deftest nrepl-server-test
  (let [dir (fs/create-temp-dir)
        cli (str (fs/absolutize "lib/cli.js"))
        proc (p/process {:dir (str dir) :out :inherit :err :inherit}
                        "node" cli "nrepl-server" "--port" "0")
        port-file (fs/file (str dir) ".nrepl-port")]
    (try
      (loop [n 0]
        (when (and (not (fs/exists? port-file)) (< n 100))
          (Thread/sleep 100)
          (recur (inc n))))
      (is (fs/exists? port-file) "server wrote .nrepl-port")
      (let [port (parse-long (str/trim (slurp port-file)))]
        (with-open [socket (java.net.Socket. "127.0.0.1" (int port))]
          (let [out (.getOutputStream socket)
                in (java.io.PushbackInputStream. (.getInputStream socket))]
            (let [resp (nrepl-exchange in out {"op" "clone" "id" "1"})]
              (is (get resp "new-session")))
            (let [resp (nrepl-exchange in out {"op" "eval" "id" "2"
                                               "code" "(mapv inc [1 2 3])"})]
              (is (= "[2 3 4]" (get resp "value"))))
            (let [resp (nrepl-exchange in out {"op" "eval" "id" "3"
                                               "code" "(defn twice [x] (* 2 x)) (twice 21)"})]
              (is (= "42" (get resp "value"))))
            (let [resp (nrepl-exchange in out {"op" "complete" "id" "4"
                                               "prefix" "twi"})]
              (is (some #(= "twice" (bytes->str (get % "candidate")))
                        (get resp "completions")))))))
      (finally
        (p/destroy-tree proc)))))

(deftest macro-test
  (let [out (:out (sh {:err :inherit
                       :dir "test-resources/test_project"}
                      "npx cherry run macro_test.cljs"))]
    (is (str/includes? out "22"))
    (is (str/includes? out "1"))))

(deftest nested-macro-test
  (t/testing "a macro expansion calling another macro ns fully qualified expands, not a runtime call"
    (let [out (:out (sh {:err :inherit
                         :dir "test-resources/test_project"}
                        "npx cherry run nested_macro_test.cljs"))]
      (is (str/includes? out "25")))))

(deftest equality-test
  (let [out (:out (sh {:err :inherit
                       :dir "test-resources/test_project"}
                      "npx cherry run equality_test.cljs"))]
    (is (str/includes? out "[false true]"))))

(deftest repl-api-test
  (let [out (:out (p/shell {:out :string} "node test-resources/js_api.mjs"))]
    (is (= ["1" "1"]  (str/split-lines out)))))

(deftest compile-test
  (let [tmp-file "/tmp/cherry-compile-test.cljs"
        out-file "/tmp/cherry-compile-test.mjs"]
    (spit tmp-file "(ns compile-test)\n(defn hello [] (js/console.log \"hello\"))")
    (shell "rm" "-f" out-file)
    (sh "npx" "cherry" "compile" tmp-file)
    (is (.exists (java.io.File. out-file)) "compile should create output file")))

(deftest cross-platform-jvm-test
  (let [{:keys [exit]} (sh {:err :inherit}
                           "clojure -M:test -n cherry.cross-platform-test")]
    (is (zero? exit) "cross-platform test passes on JVM")))

(deftest cross-platform-cherry-test
  (let [out (:out (sh {:err :inherit}
                      "node lib/cli.js run test/cherry/cross_platform_test.cljc"))]
    (is (str/includes? out "0 failures") "cross-platform test passes on Cherry")))

(defn run-tests []
  (shell {:dir "test-resources/test_project"} "npm install")
  (let [{:keys [fail error]} (t/run-tests 'integration-tests)]
    (when (and fail error (pos? (+ fail error)))
      (throw (ex-info "Tests failed" {:babashka/exit 1})))))
