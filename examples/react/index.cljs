(ns index
  {:clj-kondo/config '{:config-in-call {index/$ {:linters {:invalid-arity {:level :off}}}}}}
  (:require
   ["https://cdn.skypack.dev/canvas-confetti" :as confetti]
   ["https://cdn.skypack.dev/react" :as react :refer [useEffect]]
   ["https://cdn.skypack.dev/react-dom" :as rdom]))

(defn $ [elt props children]
  (let [children (if-not children
                   props
                   children)
        elt (if (keyword? elt)
              (name elt)
              elt)]
    (react/createElement elt props children)))

(defn App []
  (useEffect (fn [] ((.-default confetti))) #js [])
  ($ :div
     ($ :code "(+ 1 2 3)")))

(rdom/render (react/createElement App) (js/document.getElementById "app"))
