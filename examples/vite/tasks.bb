(ns tasks
  (:require
   [babashka.pods :as pods]
   [babashka.tasks :refer [shell]]
   [clojure.string :as str]))

(defn watch-cljs [{:keys []}]
  (pods/load-pod "../../../pod-babashka-fswatcher/pod-babashka-fswatcher")
  (let [watch (requiring-resolve 'pod.babashka.fswatcher/watch)]
    (watch "."
           (fn [{:keys [type path]}]
             (prn type path)
             (when
                 (and (#{:write :write|chmod} type)
                      (str/ends-with? path ".cljs"))
               (shell {:continue true} "node node_modules/.bin/cherry" path))))
    @(promise)))
