(ns async)

(defn ^:async status []
  (let [resp (await (js/fetch "https://clojure.org"))
        status (await (.-status resp))]
    status))

(js/console.log "status:" (await (status)))
