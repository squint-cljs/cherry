(ns preact
  (:require ["preact" :refer [render]]
            ["preact/hooks" :refer [useState]]
            [ui]))

;; Preact via cherry's :jsx-runtime (set in cherry.edn). `#jsx` compiles to
;; jsx()/jsxs() calls that import preact's runtime directly, so there's no
;; separate JSX transform step, and it works at the REPL too. Styles cross the
;; JS boundary via clj->js, like CLJS with react. useState drives re-render;
;; the framework owns reactivity (no manual watch).
(defn App []
  (let [[n set-n] (useState 0)]
    #jsx [:div {:style (clj->js ui/card)}
          [:div {:style (clj->js (assoc ui/label :color "#673ab8"))} "preact - #jsx"]
          [:div {:style (clj->js ui/counted)} "Counted: " n]
          [:button {:style (clj->js ui/btn) :onClick #(set-n (inc n))} "Click me!"]]))

(render #jsx [App] (js/document.querySelector "#preact"))

;; preact reconciles: the App instance and its useState survive the re-render
(defn ^:dev/after-load re-render [] (render #jsx [App] (js/document.querySelector "#preact")))
