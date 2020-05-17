(ns mathplot.core
  (:gen-class)
  (:use [seesaw core font color graphics])
  (:require [mathplot.parse :refer [string->fn]]
            [mathplot.helpers :refer [unmap centering]]
            [mathplot.shape :refer [shape->paint new-fn-plot]]
            [swinghelp.core :refer [sset-class! sset! sget]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; state

(def plot-modes #{:explicit :parameter})

(def state-init
  {:canvas-width 500
   :canvas-height 500
   :shapes []
   :diff [0 0]
   :mode :explicit
   :font-size 30
   })

(def state (atom state-init))

(defn- reset-state-keys! [& ks]
  (reset! state (merge @state (select-keys state-init ks))))

(defn- reset-state! [] (reset! state state-init) )

(defmulti update-root-id
  "[root id]

  Updates root in the component specified by id."
  (fn [root id] id))

(defn- update-root [root & ks]
  (dorun (map #(update-root-id root %) ks))
  root)

(defn- add-shapes! [& shapes]
  (swap! state update :shapes concat shapes))

;; state object

;;(unmap state->object)

(defmulti state->object
  "[state-val id]

  Takes current state value and id ,returns corresponding object."
  (fn [state-val id] id))

;; should be in shape module?
(defmethod state->object :axes
  [{w :canvas-width h :canvas-height [x y] :diff} _]
  (fn [c g]
    (let [the-style (style :foreground (color "black")) ]
      (.setSize c w h)
      (centering g w h
                 (draw g (line (- w) y w y) the-style)
                 (draw g (line x (- h) x h) the-style)))))

(defmethod state->object :shapes
  [v _]
  (let [paint-fns (map #(shape->paint @state %) (:shapes @state))]
    (fn [c g]
      (dorun (map (fn [paint] (paint c g)) paint-fns)))))

(defmethod update-root-id :shapes [root _]
  (sset! root [:paint :paint] (state->object @state :shapes )))

(defmethod update-root-id :axes [root _]
  (sset! root [:paint :paint] (state->object @state :axes)))

;; translate

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
                (update-root root :axes))))
    root))


;; frame

(defn- make-frame []
  (frame :width 600
         :height 600
         :content (border-panel
                   :id :main
                   :center (canvas :id :paint))))

;; input ui

(def input-ui-table
  (let [input-explicit (fn [e]
                         (if-let [f (string->fn (text e))]
                           (do (add-shapes! (new-fn-plot f ))
                               (update-root (to-root e) :shapes))
                           (alert (format "incorrect input: %s" (text e)))))]
    {:explicit (horizontal-panel
                :items
                [(label :text "f(x)" :class :text)
                 (text :id :input-explicit
                       :class :text
                       :listen
                       [:action input-explicit])])}))

(defmethod state->object :input-ui [{:keys [mode]} _]
  (get input-ui-table mode))

(defmethod update-root-id :input-ui [root _]
  (sset! root [:main :north] (state->object @state :input-ui)))

(defmethod update-root-id :font-size [root _]
  (sset-class! root [:text :font] (font :size(:font-size @state))))

;; demo

(defn- run []
  (reset-state!)
  (-> (make-frame)
      (update-root :input-ui :font-size)
      add-translate-behavior
      show!))

;; (run)
