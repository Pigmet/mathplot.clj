(ns mathplot.core
  (:gen-class)
  (:use [seesaw core font color graphics])
  (:require [mathplot.parse :refer [string->fn]]
            [mathplot.helpers :refer [unmap centering]]
            [mathplot.shape :refer [shape->paint new-fn-plot new-parameter-plot]]
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
   })

(def state (atom state-init))

(defn- reset-state-keys! [& ks]
  (reset! state (merge @state (select-keys state-init ks))))

(defn- reset-state! [] (reset! state state-init) )

(unmap update-root-id)

(defmulti update-root-id
  "[root id]

  Returns fn that updates root in the component specified by id."
  identity)

(defn- update-root
  "Updates root according to ks. Returns root."
  [root & ks]
  (->> ks
       (map update-root-id)
       (map (fn [f] (f root)))
       dorun)
  root)

(defn- add-shapes! [& shapes]
  (swap! state update :shapes concat shapes))

;; state object

(defmulti state->object
  "[state-val id]

  Takes current state value and id ,returns corresponding object."
  (fn [state-val id] id))

(defmethod state->object :shapes
  [v _]
  (let [paint-fns (map #(shape->paint @state %) (:shapes @state))]
    (fn [c g]
      (dorun (map (fn [paint] (paint c g)) paint-fns)))))

(defmethod update-root-id :shapes [_]
  (fn [root]  (sset! root [:paint :paint] (state->object @state :shapes ))))

;; translate yay yay yay 

(defn- get-pos [e] [(.getX e)(.getY e)])

(defn- add-translate-behavior [root]
  (let [a (atom {:start [0 0] :end [0 0] :diff [0 0]})]
    (listen (sget root :paint)
            :mouse-pressed
            (fn [e]
              (swap! a assoc :start (get-pos e) :end (get-pos e)
                     :diff [0 0]))
            :mouse-dragged
            (fn [e]
              (swap! a assoc :end (get-pos e))
              (let [{:keys [start end]} @a
                    flip-y (fn [[x y]] [x ( - y)])
                    start (flip-y start)
                    end (flip-y end)
                    diff1 (map - end start)
                    diff2 (map - (map - end start) (:diff @a) )
                    ret (map + (:diff @state) diff2)]
                (swap! a assoc :diff diff1)
                (swap! state assoc :diff ret)
                (update-root root :shapes))))
    root))

;; frame

(defn- make-frame []
  (frame :width 600
         :height 600
         :content
         (border-panel
          :north
          (vertical-panel
           :items [(horizontal-panel :id :mode-select)
                   (horizontal-panel :id :buttons)])
          :center
          (border-panel
           :id :main
           :center (canvas :id :paint)))))

;; buttons

(defn- make-buttons []
  (->> [:reset :close]
       (map #(button :id % :text (name %) :class :text))))

;; input ui

(defn- mode-select-part []
  (let [group (button-group)
        panel (horizontal-panel
               :items (->> plot-modes
                           (map #(radio :text (name %)
                                        :class :text
                                        :group group))))]
    (listen group :action
            (fn [e]
              (let [mode (-> e selection text keyword)
                    root (to-root e)]
                (swap! state assoc :mode mode)
                (update-root root :input-ui))))
    panel))

(defn- input-explicit [e]
  [e]
  (if-let [f (string->fn (text e))]
    (do (add-shapes! (new-fn-plot f ))
        (update-root (to-root e) :shapes))
    (alert (format "incorrect input: %s" (text e)))))

(defn- input-parameter [e]
  (let [root (to-root e)
        xfn-s (sget root [:xfn :text])
        yfn-s (sget root [:yfn :text])
        xfn (string->fn xfn-s)
        yfn (string->fn yfn-s)]
    (println xfn-s yfn-s)
    (if (and xfn yfn)
      (do
        (add-shapes! (new-parameter-plot xfn yfn))
        (update-root root :shapes))
      (alert (format "incorrect input: xfn %s, yfn %s" xfn-s yfn-s)))))

(def input-ui-table
  {:explicit
   (horizontal-panel
    :items
    [(label :text "f(x)" :class :text)
     (text :id :input-explicit
           :class :text
           :listen
           [:action input-explicit])])
   :parameter
   (vertical-panel
    :items
    (->> [["x" :xfn] ["y" :yfn]]
         (map (fn [[s id]]
                (horizontal-panel
                 :items
                 [(label :text s :class :text)
                  (text :id id :class :text
                        :listen
                        [:action input-parameter])])))))})


(defmethod state->object :input-ui [{:keys [mode]} _]
  (get input-ui-table mode))

(defmethod update-root-id :input-ui [_]
  (fn [root]
    (sset! root [:main :north] (state->object @state :input-ui))))

(defmethod update-root-id :font-size [ _]
  (fn [root]
    (sset-class! root [:text :font] (font :size(:font-size @state)))))

;; action

(defn- add-button-behavior [root]
  (->>{:close (fn [e] (dispose! root))
       :reset (fn [e] (reset-state!)
                (update-root root :shapes))}
      (map (fn [[k v]]
             (listen (sget root k) :mouse-clicked v)))
      dorun)
  root)

;; main 

(defn- build []
  (-> (make-frame)
      (sset! [:buttons :items] (make-buttons))
      (sset! [:mode-select :items] [(mode-select-part)])
      (update-root :input-ui :font-size :shapes)))

(defn- run []
  (reset-state!)
  (-> (build)
      add-translate-behavior
      add-button-behavior      
      show!))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (run))


(comment

  (do
    (swap! state assoc :mode :parameter)
    (-> (build)
        add-translate-behavior
        add-button-behavior      
        show!))

  )

;; (run)
