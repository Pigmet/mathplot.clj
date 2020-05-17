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

(def state-init
  {:canvas-width 500
   :canvas-height 500
   :diff [0 0]
   })

(def state (atom state-init))

(defn- reset-state-keys! [& ks]
  (reset! state (merge @state (select-keys state-init ks))))

(defn- reset-state! [] (reset! state state-init) )

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

(defmethod state->object :axes [{w :canvas-width h :canvas-height } _]
  (fn [c g]
    (let [the-style (style :foreground (color "black")) ]
      (.setSize c w h)
      (push g
            (translate g (/ w 2) (/ h 2))
            (scale g 1 -1)
            (draw g (line (- w) 0 w 0) the-style)
            (draw g (line 0 (- h) 0 h) the-style)))))

;; frame

(defn- make-frame []
  (frame :width 600
         :height 600
         :content (border-panel
                   :id :main
                   :center (canvas :id :paint))))

(comment

  (-> (make-frame)
      (sset! [:paint :paint] (state->object @state :axes))
      show!)

  )
