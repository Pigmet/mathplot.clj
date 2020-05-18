(ns mathplot.temp
  (:require [swinghelp.core :refer :all])
  (:use [seesaw core font color ]))

(defn- make-frame []
  (frame :width 500
         :height 500
         :content (border-panel :id :main
                                :center (label :id :out))))

(defn- select-part []
  (let [group (button-group)
        id->text {:explicit "f(x)" :parameter "parameter"}
        panel (horizontal-panel
               :items
               (->> [:explicit :parameter]
                    (map #(radio :text (name %) :group group))))]
    (listen group :action
            (fn [e]
              (let [s (-> group selection text keyword id->text)]
                (text! (sget (to-root e) :out) s))))
    panel))

(defn run []
  (-> (make-frame)
      (sset! [:main :north] (horizontal-panel
                             :items [(select-part)]))
      show!))

;; (run)
