(ns mathplot.helpers
  (:use [seesaw core graphics color])
  (:require [clojure.spec.alpha :as s]))

(defmacro unmap
  "Same as (do (ns-unmap *ns* arg1) (ns-unmap *ns* arg2) ...)"
  [& args]
  `(do ~@(map (fn [name#] `(ns-unmap ~'*ns* '~name#)) args)))

(defn split-by [pred coll]
  (->> coll
       (reduce (fn [[m n] x]
                 (if (pred x)
                   [(assoc m (inc n) [] ) (inc n)]
                   [(update m n conj x) n]))
               [{0 []} 0])
       first
       vals
       (filter seq)))

(defmacro nan-when-throw [& body]
  `(try ~@body (catch ArithmeticException ex# Double/NaN)))

(defn is-nan? [x]
  (and (number? x)(Double/isNaN x)))

(defn is-finite? [x] (and (number? x) (Double/isFinite x)))

(defn safe-swap!
  "Updates the value of atom only if the result satisfies pred."
  [a pred f & args]
  (let [new-val (apply f @a args)]
    (if (pred new-val) (reset! a new-val) @a)))

;; r,g,b, <= 200, a = 200 looks ok.

(defn rand-color []
  (let [[r g b] (repeatedly 3 #(rand-nth (range 0 200)))]
    (color r g b 200)))

(defmacro centering [g-sym width-sym height-sym & body]
  `(push ~(symbol g-sym)
         (translate ~(symbol g-sym)
                    (/ ~(symbol width-sym) 2)
                    (/ ~(symbol height-sym) 2))
         (scale ~(symbol g-sym) 1 -1)
         ~@body))
