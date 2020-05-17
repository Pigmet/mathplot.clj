(ns mathplot.parse
  (:require
   [instaparse.core :as insta]
   [clojure.string :refer [join]]
   [mathplot.helpers :refer [nan-when-throw]]))

;; Convert string to function, e.g., parse string like x + 1 to
;; a Clojure fn: (fn [x] (+ x 1)).

;; funtion operation

(defn- function-op-factory
  "Takes numeric operator such as +, returns fn
  that takes variable number of function arguments and
  returns fn that is the result of applying op to the given fns."
  [op]
  (let [binary-handler (fn [f g] (fn [x] (op (f x) (g x))))]
    (fn
      ([f] (fn [x] (op (f x))))
      ([f g] (binary-handler f g))
      ([f g & more] (reduce binary-handler (binary-handler f g) more)))))

;; these are functions that return another function

(def fn-plus (function-op-factory +))

(def fn-minus (function-op-factory -))

(def fn-mul (function-op-factory *))

(def fn-div (function-op-factory
             (fn [& args] (nan-when-throw (apply / args)))))

;; parse

(def ^:private the-grammar 
  "<EXP> = op f op

  <f> = f-op | f-negate | const | identity  | powered | special | left op f op right

  special = ('log' | 'sin' | 'cos' | 'tan' | 'exp') op left op f op right 
  f-negate = <minus> op f
  f-op = f op (add | mul | div | minus) op f
  powered  = f op power op left op f op right
  identity = var
  const = number
  
  <left> = <'(' | '{' | '['>
  <right> = <')' | '}' |  ']'>

  <power> = <'**'>
  <var> = <'x'>

  <add> = '+'
  <mul> = '*'
  <div> = '/'
  <minus> = '-'

  <op> = <whitespace*>
  <wp> = <whitespace+>
  whitespace = #'\\s+'
  word = letter+

  number =  digit+
  <letter> = #'[a-zA-Z]'
  <digit> = #'[0-9]'
  ")

(defn- compile-parser [] (insta/parser the-grammar))

(defn- parse-string-1
  "Parses string s by the-grammar. No transform is applied."
  [s]
  ((compile-parser) s))

;;(ditch create-fn)

(defmulti create-fn :type)

;; const and identity are the basis of recursion, so to say.

(defmethod create-fn :const [{:keys [v]}] (constantly v))

(defmethod create-fn :identity [_] (fn [x] x))

(defmethod create-fn :powered [{:keys [base power]}]
  (let [[base power] (map create-fn [base power])] 
    (fn [x] (Math/pow (base x) (power x) ))))

(def ^:private function-op-table
  {"+" fn-plus
   "-" fn-minus
   "*" fn-mul
   "/" fn-div})

(defmethod create-fn :f-op [{:keys [op args]}]
  (let [[x y] (map create-fn args)
        the-op (function-op-table op)]
    (the-op x y)))

(defmethod create-fn :f-negate [{:keys [v]}]
  (let [v (create-fn v)]
    (fn [x] (- (v x)))))

(def ^:private
  special-fn-table
  {:log #(Math/log %)
   :sin #(Math/sin %)
   :cos #(Math/cos %)
   :tan #(Math/tan %)
   :exp #(Math/exp %)})

(defmethod create-fn :special [{:keys [f v]}]
  (let [v (create-fn v)
        the-f (special-fn-table f)]
    (fn [x] (the-f (v x)))))

(def ^:private transform-map
  {:number (fn [& args] (read-string (join args)))
   :const (fn [v] {:type :const :v v})
   :identity (fn [] {:type :identity})
   :f-negate (fn [v] {:type :f-negate :v v})
   :powered (fn [x y] {:type :powered :base x :power y})
   :f-op (fn [x op y] {:type :f-op :op op :args [x y]})
   :special (fn [x y] {:type :special :f (keyword x) :v y})})

(defn- parse-string [s]
  (->> (parse-string-1 s) (insta/transform transform-map)))

(defn string->fn
  "Takes string, converts it to fn or nil on parse error."
  [s]
  (let [res (parse-string s)]
    (when-not (insta/failure? res)
      (create-fn (first res)))))

