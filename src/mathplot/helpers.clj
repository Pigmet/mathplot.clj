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

(defn scale-fn [f sx sy]
  (fn [x] (* sy (f (/ x  sx)))))

(defn translate-fn [f dx dy]
  (fn [x] (+ dy (f (- x dx)))))

(s/def ::coll-lines-arg-spec
  (s/or :coll-ps (s/coll-of (s/cat :x  number? :y number?))
        :coll-coll-ps (s/coll-of (s/coll-of (s/cat :x number? :y number?)))))

(defn- coll->lines-1 [coll-ps]
  (->> coll-ps
       (partition 2 1)
       flatten
       (partition 4)
       (map #(apply line %))))

(defn- coll->lines
  "Given coll of 2d points, or coll of such coll,
  returns coll of lines connecting the points in data."
  [data]
  (let [arg-type (key (s/conform ::coll-lines-arg-spec data))]
    (println arg-type)
    (case arg-type
      :coll-ps (coll->lines-1 data)
      :coll-coll-ps (mapcat coll->lines-1 data))))

(def interval-step 1000)

(defn interval
  ([width] (interval (- width) width))
  ([low high]
   (range low high (/ (- high low) interval-step))))

(defmacro def-
  "same as def, yielding non-public def"
  [name & decls]
  (list* `def (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defmulti-
  "same as def, yielding non-public defmulti"
  [name & decls]
  (list* `defmulti (with-meta name (assoc (meta name) :private true)) decls))

(defmacro print-ex [& body]
  `(try ~@body (catch Exception ex#
                 (println "caught exception:" (.getMessage ex#)))))

(defmacro push-center [c-sym g-sym & body]
  `(push ~(symbol g-sym)
         (translate ~(symbol g-sym)
                    (/ (width ~(symbol c-sym)  ) 2)
                    (/ (height ~(symbol c-sym)  ) 2))
         (scale ~(symbol g-sym) 1 -1)
         ~@body))

(defn safe-swap!
  "Updates the value of atom only if the result satisfies pred."
  [a pred f & args]
  (let [new-val (apply f @a args)]
    (if (pred new-val) (reset! a new-val) @a)))

;; r,g,b, <= 200, a = 200 looks ok.

(defn rand-color []
  (let [[r g b] (repeatedly 3 #(rand-nth (range 0 200)))]
    (color r g b 200)))

