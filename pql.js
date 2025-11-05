import cyntax from "./cyntax.js"

export default cyntax({})`

(defn from [source] {
  "keys" (keys source)
  "indices" (into [] (range 0 (count ((vals source) 0))))
  "data" source })

; should extract core group by
(defn group-by [key df]
  (let [groups (fold
    (fn [r index] (update r (get-in df ["data" key index]) (fn [val] (if val (conj val index) [index]))))
    {}
    (df "indices"))]
  
  (vals groups)))

(defn sum [key df] (fold + 0 ((df "data") key)))

(def data {
  "name" ["Alice", "Bob", "Charles", "Doris", "Ellie", "Freya"]
  "city" ["Amsterdam" "Bristol" "Amsterdam" "Cardiff" "Cardiff" "Bristol"]
  "pay" [1, 2, 3, 4, 5, 6] })

(defn main [] (->> (from data)
  (group-by "city")))

`
