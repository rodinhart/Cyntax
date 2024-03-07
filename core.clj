;; lang
(def defn (macro [name & forms]
  '(def ~name (fn ~@forms))))

(def defmacro (macro [name & forms]
  '(def ~name (macro ~@forms))))

(defmacro ->
  ([x] x)
  ([x form] (let [s (seq form)] '(~(first s) ~x ~@(rest s))))
  ([x form & rest] '(-> (-> ~x ~form) ~@rest)))

(defn assoc-in [map keys val]
  (if (= (count keys) 0)
    val
    (assoc map (keys 0) (assoc-in (map (keys 0)) (slice keys 1) val))))

(defmacro cond
  ([] nil)
  ([test expr & rest] '(if ~test ~expr (cond ~@rest))))

(defmacro not= [lhs rhs] '(not (= ~lhs ~rhs)))

(defn | [lhs rhs] (rhs lhs))

(defmacro |>
  ([a] a)
  ([a b & rest] '(|> (| ~a ~b) ~@rest)))

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

(deftype ArraySeq [arr i])

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

(defn map-array [f arr]
  (into [] (map f arr)))

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

(def fib (lazy-cons 1 (lazy-cons 1 (map + fib (drop 1 fib)))))

;; PQLisp
(defn fst [tuple] (tuple 0))
(defn snd [tuple] (tuple 1))

;; destructure keys and data
(defn create-obj [dataframe index]
  (into {} (map (fn [key] (let [
    column ((dataframe "data") key)]
    
    [key (column index)])) (dataframe "keys"))))

(defn scope-expression [expr]
  '(fn [scope] ~(scope-identifiers expr)))

;; needs recursion on data structures
(defn scope-identifiers [expr]
  (cond
    (and
      (symbol? expr)
      (let [s (string expr)] (not= (upper-case s) s))) '(scope ~(string expr))
    (list? expr) (into () (into () (map scope-identifiers expr)))
    true expr))

; awkward having hashmap appear
; also do with destructuring for pairs
(defn scope-tuple [tuple] { "hashmap"
  (into [] (map (fn [p] [(fst p) (scope-expression (snd p))]) (tuple "hashmap"))) })


(defmacro FILTER [expr]
  '(FILTER* ~(scope-expression expr)))

(defn FILTER* [predicate]
  (fn [scope] (let [
    dataframe (scope "self")
    p (fn [index] (predicate (create-obj dataframe index)))
    result (into [] (filter p (dataframe "indices")))]

    (assoc scope
      "self" (assoc dataframe "indices" result)))))

(defmacro FROM [expr] '(FROM* ~(scope-identifiers expr)))

(defn FROM* [data] { "self" {
  "data" data
  "keys" (into [] (map fst data))
  "indices" (into [] (range 0 (fold max 0 (map (comp count snd) data))))
}})

(defmacro DERIVE [calcs] '(DERIVE* ~(scope-tuple calcs)))

(defn DERIVE* [calcs]
  (fn [scope] (let [
    dataframe (scope "self")
    rf (fn [df p] (let [
      key (fst p)
      fun (fn [index] ((snd p) (assoc (create-obj df index) "self" df )))]
      
      (-> df
        (assoc "keys" (conj (df "keys") key))
        ; assoc-in?
        (assoc-in ["data" key] (map-array fun (df "indices"))))))]
    (assoc scope "self" (fold rf dataframe calcs)))))

;; yuk
(defmacro SUM [expr] '((SUM* (fn [scope] ~expr)) scope))

(defn SUM* [fun]
  (fn [scope] (let [
    dataframe (scope "self")
    rf (fn [r index] (+ r (fun (create-obj dataframe index))))
    sum (fold rf 0 (dataframe "indices"))]
    
    sum)))


(def data {
  "x" (into [] (take 10 (cycle ["I", "II"])))
  "y" (into [] (range 1 11))
})

((fn [scope]
  (|>
    (FROM data)
    (FILTER (< y 6))
    (DERIVE { "r" (* y y), "s" (SUM y) })
  )
) { "data" data })

; a * b          -> ($) => $["a"] * $["b"]
; FILTER (= a b) -> ($) => FILTER(($) => $["a"] === $["b"], $)
; SUM a          -> ($) => SUM(($) => $["a"], $)                NO!?
;                -> ($) => SUM("a", $)
; 12 * SUM a     -> ($) => 12 * SUM("a", $)

; if we didn't have custom scope, users could access core functions
; well, could they? If all non-identifiers were UPPERCASE, and only
; known operators are allowed, we'd be good?
; anything with foo* is internal? Macros are not an issue, can be named foo?