import core from "./core.js"
import { lisp, native } from "./lisp.js"

lisp({ ...native, ...core })`

(test (-> 12 (/ 3) (- 1)) 3)
(test (-> 12 (/ 3) -) -4)

(test (->> 12 (/ 48) (- 1)) -3)
(test (->> 12 (/ 48) -) -4)

(test (| 21 (fn [x] (/ x 3)) (fn [y] (- y 5))) 2)

(test (and false (fubar 2)) false)
(test (and 42 "Hello") "Hello")

(test
  (assoc-in { "a" { "b" 2 } } ["a" "c"] 3)
  { "a" { "b" 2 "c" 3 } })
(test
  (assoc-in nil ["b" "d"] 4)
  { "b" { "d" 4 } })

(test ((comp (fn [x] (* 2 x)) +)  2 3 5) 20)

(test
  (cond (< 1 2) "Less" true (fubar 2)) "Less")
(test (cond (< 2 1) "Less" true "More") "More")

(test ((constantly 4) 2 3 5) 4)

(test (identity 'blah) 'blah)

(test (inc 10) 11)

(test (not= 2 2) false)

(test (odd? 3) true)
(test (odd? 4) false)

(test
  (update { "a" 2 } "a" + 3)
  { "a" 5 })

(test
  (update-in { "a" { "c" 6 } } ["a" "c"] + 7)
  { "a" { "c" 13 } })
(test
  (update-in nil ["a" "b"] array 1)
  { "a" { "b" [nil 1] } })

(test (get-in { "a" { "b" 10 } } ["a" "b"]) 10)


(test (conj nil 2) '(2))
(test (count nil) 0)
(test (seq nil) nil)

(test (conj '(3 5) 2) '(2 3 5))
(test (count '(2 3 5 7)) 4)
(test (seq '(2 3)) '(2 3))


(test (first '(2 3 5)) 2)
(test (rest '(2 3 5)) '(3 5))


(test
  (into [] (take 5 (cycle [2 3 5])))
  [2 3 5 2 3])

(test
  (into [] (drop 2 [2 3 5 7]))
  [5 7])

(test
  (into [] (filter odd? [1 2 3 4 5 6]))
  [1 3 5])

(test (fold + 100 [1 2 3 4 5]) 115)

(test
  (into [] (map inc [2 3 5]))
  [3 4 6])

(test
  (into [] (map + [1 2 4] [8 16]))
  [9 18])

(test (map-array inc [2 3 5]) [3 4 6])

(test
  (into [] (range 6))
  [0 1 2 3 4 5])

(test
  (into [] (range 2 6))
  [2 3 4 5])

(test
  (into [] (take 5 (range 10)))
  [0 1 2 3 4])
`

export default core
