import { lisp, native } from "./lisp.js"

export default lisp(native)`

;; lang
(def defmacro (macro [name & forms]
  '(def ~name (macro ~@forms))))

(defmacro defn [name & forms]
  '(def ~name (fn ~@forms)))

(defmacro ->
  ([x] x)
  ([x form] (if (list? form)
    '(~(car form) ~x ~@(cdr form))
    '(~form ~x)))
  ([x form & rest] '(-> (-> ~x ~form) ~@rest)))

(defmacro ->>
  ([x] x)
  ([x form] (if (list? form)
    '(~@form ~x)
    '(~form ~x)))
  ([x form & rest] '(->> (->> ~x ~form) ~@rest)))

(defn | [val & pipes]
  (loop [cur val i 0]
    (if (< i (count pipes))
      (recur ((pipes i) cur) (+ i 1))
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

(defn comp
  ([] identity)
  ([f] f)
  ([g f] (fn
    ([] (g (f)))
    ([x] (g (f x)))
    ([x y] (g (f x y)))
    ([x y z] (g (f x y z))))))

(defmacro cond
  ([] nil)
  ([test expr & rest] '(if ~test ~expr (cond ~@rest))))

(defn constantly [x] (fn [& args] x))

(defn get
  ([map key] (get* map key))
  ([map key default] (let [tmp (get* map key)] (if (nil? tmp) default tmp))))

(defn get-in [map keys]
  (loop [r map i 0]
    (if (< i (count keys))
      (recur (r (keys i)) (+ i 1))
      r)))

(defn identity [x] x)

(defn inc [x] (+ x 1))

(defn nil? [x] (= x nil))

(defn not= [lhs rhs] (not (= lhs rhs)))

(defn odd? [x] (= (% x 2) 1))

(defn slice
  ([arr start] (slice* arr start nil))
  ([arr start end] (slice* arr start end)))

(defn tap [x] (let [tmp (log x)] x)) ; depends on log

(defn update
  ([map key f] (assoc map key (f (map key))))
  ([map key f x] (assoc map key (f (map key) x))))

(defn update-in
  ([map keys f] (assoc-in map keys (f (get-in map keys))))
  ([map keys f x] (assoc-in map keys (f (get-in map keys) x))))


(defmacro test [form expected]
  '(test* (quote ~form) ~form ~expected))


;; types
(deftype Nil [])

(deftype LazyCons [car cdr])

(defmacro lazy-cons [car cdr]
  '(LazyCons ~car (fn [] ~cdr)))


;; Fn
(defprotocol Fn
  (apply [args]))

(extend Nil Fn
  (apply [] nil))


;; collections
(defprotocol Coll
  (conj [item])
  (count [])
  (seq []))

(extend Nil Coll
  (conj [item] (cons item nil))
  (count [] 0)
  (seq [] nil))

(extend List Coll
  (conj [item] (cons item (cons car cdr)))
  (count [] (+ 1 (if cdr (count cdr) 0)))
  (seq [] (List car cdr)))

(extend ArraySeq Coll
  (conj [item] (ArraySeq (conj arr item) i))
  (count [] (count arr))
  (seq [] (ArraySeq arr i)))

(extend LazyCons Coll
  (conj [item] (lazy-cons item (LazyCons car cdr)))
  (count [] (+ 1 (let [_ (cdr)] (if _ (count _) 0))))
  (seq [] (LazyCons car cdr)))


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

(extend LazyCons Seq
  (first [] car)
  (rest [] (cdr)))


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

(defn filter-array [p arr] (into [] (filter p arr)))

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
        nil))]
    (help (seq coll1) (seq coll2)))))

(defn map-array
  ([f arr] (into [] (map f arr)))
  ([f arr1 arr2] (into [] (map f arr1 arr2))))

(defn map-list [f list]
  (if list
    (cons (f (car list)) (map-list f (cdr list)))
    nil))

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
