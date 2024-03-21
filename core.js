import { lisp, native } from "./lisp.js"

export default lisp(native)`

;; lang
(def defn (macro [name & forms]
  '(def ~name (fn ~@forms))))

(def defmacro (macro [name & forms]
  '(def ~name (macro ~@forms))))

(defmacro ->
  ([x] x)
  ([x form] (let [s (seq form)] '(~(first s) ~x ~@(rest s))))
  ([x form & rest] '(-> (-> ~x ~form) ~@rest)))

(defn | [val & pipes]
  (loop [cur val s (seq pipes)]
    (if s
      (recur ((first s) cur) (rest s))
      cur)))

(defmacro and
  ([] true)
  ([x] x)
  ([x & rest]
    '(let [and# ~x]
      (if and# (and ~@rest) and#))))

(defn assoc-in [map keys val]
  (if (= (count keys) 0)
    val
    (assoc map (keys 0) (assoc-in (map (keys 0)) (slice keys 1) val))))

(defmacro cond
  ([] nil)
  ([test expr & rest] '(if ~test ~expr (cond ~@rest))))

(defn constantly [x] (fn [& args] x))

(defn not= [lhs rhs] (not (= lhs rhs)))

(defn update [map key f x]
  (assoc map key (f (map key) x)))

(defn update-in [map keys f x]
  (assoc-in map keys (f (get-in map keys) x)))

;; could use loop?
(defn get-in [map keys]
  (if (= (count keys) 0)
    map
    (get-in (map (keys 0)) (slice keys 1))))

;; collections
(extend Null Coll
  (conj [item] (cons item nil))
  (count [] 0)
  (seq [] nil))

(extend List Coll
  (conj [item] (cons item (cons car cdr)))
  (count [] (+ 1 (if cdr (count cdr) 0)))
  (seq [] (List car cdr)))

;; sequences
(defprotocol Seq
  (first [])
  (rest []))

(extend List Seq
  (first [] car)
  (rest [] cdr))

(extend ArraySeq Seq
  (first [] (arr i))
  (rest [] (if (< (+ i 1) (count arr)) (ArraySeq arr (+ i 1)) nil)))

(deftype LazyCons [car cdr])

(extend LazyCons Coll
  (conj [item] (lazy-cons item (LazyCons car cdr)))
  (count [] (+ 1 (if (cdr) (count (cdr)) 0))) ; use let
  (seq [] (LazyCons car cdr)))

(extend LazyCons Seq
  (first [] car)
  (rest [] (cdr)))

(defmacro lazy-cons [car cdr]
  '(LazyCons ~car (fn [] ~cdr)))

;; sequence operations
(defn cycle [coll]
  (let [
    help (fn [s]
      (if s
        (lazy-cons (first s) (help (rest s)))
        (help (seq coll))))]
  (help (seq coll))))

(defn drop [n coll]
  (let [
    help (fn [n s]
      (if (and s (> n 0))
        (help (- n 1) (rest s))
        s))]
    (help n (seq coll))))

(defn filter [p coll]
  (let [
    help (fn [s] (if s
      (if (p (first s))
        (lazy-cons (first s) (help (rest s)))
        (help (rest s)))
      s))]

    (help (seq coll))))

(defn fold [rf init coll]
  (loop [result init s (seq coll)]
    (if s
      (recur (rf result (first s)) (rest s))
      result)))

(defn into [target coll]
  (let [
    help (fn [target s]
      (if s
        (help (conj target (first s)) (rest s))
        target))]
    (help target (seq coll))))

(defn map
  ([f coll] (let [
    help (fn [s]
      (if s
        (lazy-cons (f (first s)) (help (rest s)))
        s))]
    (help (seq coll))))
    
  ([f coll1 coll2] (let [
    help (fn [s1 s2]
      (if (and s1 s2)
        (lazy-cons (f (first s1) (first s2)) (help (rest s1) (rest s2)))
        s1))]
    (help (seq coll1) (seq coll2)))))

(defn map-array
  ([f arr] (into [] (map f arr)))
  ([f arr1 arr2] (into [] (map f arr1 arr2))))

(defn range
  ([end] (range 0 end))
  ([start end] (if (< start end)
    (lazy-cons start (range (+ start 1) end))))
)

(defn take [n coll]
  (let [
    t (fn [n s]
      (if (and s (> n 0))
        (lazy-cons (first s) (t (- n 1) (rest s)))
        nil))]
    (t n (seq coll))))

`
