(ns mathplot.core
  (:gen-class)
  (:use [seesaw core font color graphics])
  (:require [mathplot.parse :refer [string->fn]]
            [mathplot.helpers :refer [unmap]]
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
   :diff [0 0]
   :mode :explicit
   })

(def state (atom state-init))

(defn- reset-state-keys! [& ks]
  (reset! state (merge @state (select-keys state-init ks))))

(defn- reset-state! [] (reset! state state-init) )

(defmulti update-root-id (fn [root id] id))

(defn- update-root [root & ks]
  (dorun (map #(update-root-id root %) ks)))

;; state object

;;(unmap state->object)

(defmulti state->object
  "[state-val id]

  Takes current state value and id ,returns corresponding object."
  (fn [state-val id] id))

(defmacro centering [g-sym width-sym height-sym & body]
  `(push ~(symbol g-sym)
         (translate ~(symbol g-sym)
                    (/ ~(symbol width-sym) 2)
                    (/ ~(symbol height-sym) 2))
         (scale ~(symbol g-sym) 1 -1)
         ~@body))

(defmethod state->object :axes
  [{w :canvas-width h :canvas-height [x y] :diff} _]
  (fn [c g]
    (let [the-style (style :foreground (color "black")) ]
      (.setSize c w h)
      (centering g w h
                 (draw g (line (- w) y w y) the-style)
                 (draw g (line x (- h) x h) the-style)))))

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

;; demo

(defn- run []
  (reset-state!)
  (-> (make-frame)
      (sset! [:paint :paint] (state->object @state :axes))
      add-translate-behavior
      show!))

;; (run)
