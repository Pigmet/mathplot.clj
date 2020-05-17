(ns mathplot.shape
  (:use [seesaw color  graphics])
  (:require [mathplot.helpers :refer [split-by centering]]))

(def plot-step 0.1)

(defmulti shape->paint
  "[state-val shape]

  Returns paint fn of this shape."
  (fn [state-val shape] (:id shape)))

;; should be in shape module?
(defmethod shape->paint :axes  
  [{w :canvas-width h :canvas-height [x y] :diff} _]
  (fn [c g]
    (let [the-style (style :foreground (color "black")) ]
      (.setSize c w h)
      (centering g w h
                 (draw g (line (- w) y w y) the-style)
                 (draw g (line x (- h) x h) the-style)))))

(defn new-fn-plot [f]
  {:id :fn-plot :f f})

(defn- coll->lines [coll]
  (->> coll
       (partition 2 1)
       flatten
       (partition 4)
       (map #(apply line %))))

(defmethod shape->paint :fn-plot
  [{w :canvas-width h :canvas-height [x y] :diff
    plot-range :plot-range all-range :all-range}
   {f :f}]
  (let [s (/ w plot-range)]
    (fn [c g]
      (.setSize c w h)
      (centering g w h
                 (->> (range (- all-range) all-range plot-step)
                      (map (fn [r] [r (f r)]))
                      (map #(map * % [s s]))
                      (map #(map + % [x y]))
                      (split-by #(-> % last Double/isFinite not))
                      (mapcat coll->lines)
                      (map #(draw g % (style :foreground (color "black"))))
                      dorun)))))
