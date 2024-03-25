import cyntax from "./cyntax.js"

export default cyntax({})`

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
  fun (fn [p] [(fst p) (scope-expression (snd p))])]
  
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

(defmacro GROUP_BY 
  ([set pipeline] '(GROUP_BY* ~set ~(scope-expression pipeline) scope))
  ([set pipeline scope] '(GROUP_BY* ~set ~(scope-expression pipeline) ~scope)))

(defn GROUP_BY* [set pipeline scope]
  (let [
    dataframe (scope "self")
    key ((keys set) 0)
    rf (fn [groups index]
      (let [val (get-in dataframe ["data" key index])]
        (if (contains? groups val)
          (assoc groups val (conj (groups val) index))
          (assoc groups val [index]))))
    groups (fold rf {} (dataframe "indices"))
    new (assoc dataframe "data" { key [] "agg" [] } "keys" [key "agg"] "indices" [])
    rf (fn [result group]
      (let [
        agg (pipeline (assoc-in scope ["self" "indices"] (group 1)))]
        
        (-> result
          (update "indices" conj (count (result "indices")))
          (assoc-in ["data" key] (get-in dataframe ["data" key]))
          (update-in ["data" "agg"] conj agg))))]
  
  (assoc scope "self" (fold rf new groups))))

(def result ((PQL
  (SCOPE-|
    (FROM data)
    (GROUP_BY { "x" "x" } (SUM y))
  )
) { "data" data }))

(def result ['div { "color" "red" } "HelloWorld"])

`
