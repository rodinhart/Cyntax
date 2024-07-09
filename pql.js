import cyntax from "./cyntax.js"

export default cyntax({})`

(defn map-hashmap [f x]
  { "hashmap" (map-array f (x "hashmap"))})

(defn pql-expr [expr] '(fn [scope] ~expr))

(defn compile [expr] (cond
  (and (symbol? expr) (not= (upper-case (name expr)) (name expr))) '(scope (quote ~expr))
  (list? expr) (map-list compile expr)
  (hashmap? expr) (map-hashmap (fn [p] [(name (p 0)) (compile (p 1))]) expr)
  true expr))

(defn pql [program] (-> program
  pql-expr
  eval))

(defmacro pql|
  ([] nil)
  ([a] a)
  ([a b] '(~(pql-expr b) ~a))
  ([a b & rest] '(pql| (pql| ~a ~b) ~@rest)))


(defmacro FILTER [pred] '(FILTER* ~(pql-expr (compile pred)) scope))

(defn FILTER* [pred scope] (let [dataframe (scope "_self")]
  (update-in scope ["_self" "indices"] (fn [indices]
    (filter-array (fn [index] (pred (assoc scope "x" (get-in scope ["_self" "data" "x" index])))) indices)))))

(defmacro FROM [arg] '(FROM* scope ~(compile arg)))

(defn FROM* [scope source]
  (assoc scope "_self"
    { "data" source
      "keys" (keys source)
      "indices" (into [] (range 0 (count (first (seq (vals source)))))) }))


(def RANGE-type '[{ inclusive: [both end none start]}])

(defn identify-enums [tuple enums]
  (map-hashmap
    (fn [p] (let [key (p 0) val (p 1)]
      [key (if (contains? enums key) (name val) val)]))
    tuple))

(defmacro RANGE [options]
  '(RANGE* ~(compile (identify-enums options #{'inclusive}))))

(defn RANGE* [options] (into [] (range (get options "start" 0) (+ (get options "end" 10) (if (= (options "inclusive") "both") 1 0)))))

(def script '(pql|
  (FROM data)
  (FILTER (< x 10))
  (RANGE {end (* 2 4), inclusive both})
  ))

(defn main [scope]
  (let [run (pql script)]
    (run scope)))

`
