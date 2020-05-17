(ns mathplot.shape
  (:use [seesaw graphics])
  (:require [mathplot.helpers :refer [split-by]]))

(def plot-step 0.1)

(defmulti shape->paint (fn [state-val shape] (:id shape)))

(defn new-fn-plot [f]
  {:id :fn-plot :f f})

(defn- coll->lines [coll]
  (->> coll
       (partition 2 1)
       flatten
       (partition 4)
       (map #(apply line %))))

(defmethod shape->paint :fn-plot
  [{w :canvas-width h :canvas-height [x y] :diff} {f :f}]
  (fn [c g]
    (->> (range (- (/ w 2)) (/ w 2) plot-step)
         (map (fn [r] [r (f r)]))
         (split-by #(-> % last Double/isFinite not))
         (mapcat coll->lines))))
