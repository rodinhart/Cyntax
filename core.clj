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

(defn assoc-in [map keys val]
  (if (= (count keys) 0)
    val
    (assoc map (keys 0) (assoc-in (map (keys 0)) (slice keys 1) val))))

(defmacro cond
  ([] nil)
  ([test expr & rest] '(if ~test ~expr (cond ~@rest))))

(defn not= [lhs rhs] (not (= lhs rhs)))

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

(def fib (lazy-cons 1 (lazy-cons 1 (map + fib (drop 1 fib)))))

;; PQLisp
(defn fst [tuple] (tuple 0))
(defn snd [tuple] (tuple 1))

(defmacro SCOPE-| [val & exprs]
  (let [
    pipes (map-array (fn [expr] '(fn [scope] ~expr)) exprs)]
  
  '(| ~val ~@pipes)))

;; destructure keys and data
(defn create-obj [dataframe index]
  (into {} (map (fn [key] (let [
    column ((dataframe "data") key)]
    
    [key (column index)])) (dataframe "keys"))))

(defn scope-expression [expr]
  '(fn [scope] ~(scope-identifiers expr)))

;; needs recursion on other data structures
(defn scope-identifiers [expr]
  (cond
    (and
      (symbol? expr)
      (let [s (string expr)] (not= (upper-case s) s))) '(scope ~(string expr))

    ; change to (scope-get scope key)
    (and (list? expr) (not= (car expr) 'scope)) (into () (into () (map scope-identifiers expr)))
    
    true expr))

(defn scope-pipeline [expr] (let [
  pipeline (if (and (list? expr) (= (car expr) 'SCOPE-|))
    '(SCOPE-| (FROM self) ~@(cdr expr))
    '(SCOPE-| (FROM self) ~expr))]
  (scope-expression pipeline)))

; awkward having hashmap appear
; also do with destructuring for pairs
(defn scope-tuple [tuple] (let [
  fun (fn [p] [(fst p) (scope-pipeline (snd p))])]
  
  { "hashmap" (into [] (map fun (tuple "hashmap")))}))


(defmacro DERIVE
  ([calcs] '(DERIVE* ~(scope-tuple calcs) scope))
  ([calcs scope] '(DERIVE* ~(scope-tuple calcs) ~scope)))

(defn DERIVE* [calcs scope]
  (let [
    dataframe (scope "self")
    rf (fn [df p] (let [
      key (fst p)
      fun (fn [index i] ((snd p) (assoc (create-obj df index) "self" df "_ptr" i)))]

      (-> df
        (assoc "keys" (conj (df "keys") key))
        (assoc-in ["data" key] (map-array fun (df "indices") (range (count (df "indices"))))))))]
    
    (assoc scope "self" (fold rf dataframe calcs))))

(defmacro FILTER
  ([expr] '(FILTER* ~(scope-expression expr) scope))
  ([expr scope] '(FILTER* ~(scope-expression expr) ~scope)))

(defn FILTER* [predicate scope]
  (let [
    dataframe (scope "self")
    p (fn [index] (predicate (create-obj dataframe index)))
    result (into [] (filter p (dataframe "indices")))]

    (assoc scope
      "self" (assoc dataframe "indices" result))))

(defmacro FROM [expr] '(FROM* ~(scope-identifiers expr) scope))

(defn FROM* [data scope]
  (assoc scope "self"
    (if (contains? data "data" "keys" "indices")
      data
      {
      "data" data
      "keys" (into [] (map fst data))
      "indices" (into [] (range 0 (fold max 0 (map (comp count snd) data))))
      })))

(defmacro PQL [program] '(fn [scope] ~program))

(defmacro ROUND
  ([n] '(ROUND* ~n scope))
  ([n x] '(ROUND* ~n ~x)))

(defmacro ROWS [] '(ROWS* scope))
(defn ROWS* [scope]
  (assoc-in scope ["self" "indices"]
    (into [] (take (+ (scope "_ptr") 1) (get-in scope ["self" "indices"])))))

(defmacro SUM
  ([expr] '(SUM* ~(scope-expression expr) scope))
  ([expr scope] '(SUM* ~(scope-expression expr) ~scope)))

(defn SUM* [fun scope]
  (let [
    dataframe (scope "self")
    rf (fn [r index] (+ r (fun (create-obj dataframe index))))
    sum (fold rf 0 (dataframe "indices"))]
    
    sum))


(def data {
  "x" (into [] (take 10 (cycle ["I", "II"])))
  "y" (into [] (range 1 11))
})

((PQL
  (SCOPE-|
    (FROM data)
    (FILTER (< y 6))
    (DERIVE {
      "r" (* y y)
      "s" (SCOPE-| (ROWS) (SUM y)) ; really ROWS ..0, running total
    })
  )
) { "data" data })
