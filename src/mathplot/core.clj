(ns mathplot.core
  (:gen-class)
  (:use [seesaw core font color graphics])
  (:require [mathplot.parse :refer [string->fn]]
            [mathplot.helpers :refer [unmap centering rand-color]]
            [mathplot.shape :refer
             [shape->paint new-fn-plot new-parameter-plot]]
            [swinghelp.core :refer [sset-class! sset! sget]]))


;; TODO : add parameter plot 

;; state

(def plot-modes #{:explicit :parameter})

(def state-init
  {:canvas-width 500
   :canvas-height 500
   :shapes [{:id :axes}]
   :diff [0 0]
   :mode :explicit
   :font-size 30
   :plot-range 10
   :all-range 100
   :stroke 3
   })

(def state (atom state-init))

(defn- reset-state-keys! [& ks]
  (reset! state (merge @state (select-keys state-init ks))))

(defn- reset-state! [] (reset! state state-init) )

(defn- add-shapes! [& args]
  (swap! state update :shapes concat args))

;;  frame

(defn- make-frame []
  (frame :width 800
         :height 800
         :content (border-panel
                   :north (vertical-panel
                           :items [(horizontal-panel :id :select-mode)
                                   (horizontal-panel :id :buttons)
                                   (horizontal-panel :id :input)])
                   :center (border-panel
                            :center (canvas :id :paint)))))

(defn- select-mode []
  (let [group (button-group)
        items (->> plot-modes
                   (map #(radio :text (name %)
                                :class :text
                                :group group)))
        panel (horizontal-panel :items items)]
    (listen group :action
            (fn [e]
              (let [mode (-> group selection text keyword)]
                (swap! state assoc :mode mode)
                (update-root (to-root e) :input :font-size))))
    [panel]))

(defn- buttons []
  (->> [:reset :close]
       (map #(button :id % :text (name %) :class :text))))

(defmulti state->widget
  "[state-val id]"
  (fn [state-val id] id))

(defmulti update-root-id
  "[root id]
  Returns root."
  (fn [root id] id))

(defn- update-root [root & ids]
  (reduce (fn [acc id] (update-root-id acc id))
          root
          ids))

;; parseing

(defn- parse-parameter-input [e]
  (let [xfn-s (-> e to-root (sget [:xfn :text]))
        yfn-s (-> e to-root (sget [:yfn :text]))
        xfn (string->fn xfn-s)
        yfn (string->fn yfn-s)]
    (when (and xfn yfn) [xfn yfn])))

(def mode->input-items
  {:explicit
   [(horizontal-panel
     :items [(label :text "f(x)" :class :text)
             (text :id :explicit :class :text
                   :listen [:action (fn [e] (-> e text string->fn))])])]

   :parameter
   [(vertical-panel
     :items
     (->> [["x" :xfn] ["y" :yfn]]
          (map (fn [[s id]]
                 (horizontal-panel
                  :items [(label :text s :class :text)
                          (text :id id :class :text)])))))]})

(defmethod update-root-id :input [root _]
  (sset! root [:input :items] (-> @state :mode mode->input-items)))

(defmethod update-root-id :font-size [root _]
  (sset-class! root [:text :font] (font :size (:font-size @state))))

(defn- build [root]
  (-> root
      (sset! [:buttons :items] (buttons))
      (sset! [:select-mode :items] (select-mode))
      (update-root :input :font-size)))

(defn- run []
  (-> (make-frame)
      build
      show!))

;; (run)
