(ns scratch (:require ["foo" :as foo]))
(defn App [] #jsx [foo/c #js {:x 1}])
