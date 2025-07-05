(ns counter
  (:require ["./cherry.mjs" :refer [my-cool-fn]]))

(defn setup-counter [element]
  (let [counter (atom 0)
        set-counter (fn [count]
                      (reset! counter count)
                      (set! (.-innerHTML element)
                            (str "Click! " (my-cool-fn @counter))))]
    (.addEventListener element "click"
                       #(set-counter (swap! counter inc)))
    (set-counter 0)))
