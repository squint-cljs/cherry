(ns pages.component
  (:require ["react" :refer [useState]]))

(defn Counter [^:js {:keys [init]}]
  (let [[counter setCount] (useState init)]
    #jsx [:div
          "Count:" (.join (into-array (range counter)) " " )
          [:div
           [:button
            {:onClick (fn []
                        (setCount (inc counter)))}
            "Click me"]]]))

(defn MyComponent []
  #jsx [Counter #js {:init 10}])

(def default MyComponent)
