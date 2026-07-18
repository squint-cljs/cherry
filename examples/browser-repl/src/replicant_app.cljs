(ns replicant-app
  (:require [replicant.dom :as r]
            [ui]
            [shared :as shared]))

;; Replicant renders hiccup built from real cljs data structures - cherry's
;; native fit. State is an atom; add-watch + render re-renders on change.
(defonce state (atom {:counter 0}))

(defn counter []
  [:div {:style ui/card}
   [:div {:style (assoc ui/label :color "#d2691e")} "replicant - hiccup"]
   [:div {:style ui/counted} "Counted: " (:counter @state)]
   [:button {:style ui/btn
             :on {:click (fn [_] (swap! state update :counter inc))}}
    "Click me!"]
   [:div [:code "Shared: " (shared/shared-function)]]])

(defn render []
  (r/render (js/document.querySelector "#replicant") (counter)))

(add-watch state ::render (fn [_ _ _ _] (render)))
(render)

(defn ^:dev/after-load re-render [] (render))
