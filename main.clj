;; Source: http://www.4clojure.com
;; Task #33
;; Write a function which replicates each element of a sequence a variable number of times.
;;
;; (= (__ [1 2 3] 2) '(1 1 2 2 3 3))

(fn fun1 [myList n]
  (reduce #(apply conj %1 (loop [i 0 list1 '()]
                            (if (< i n) (recur (inc i) (conj list1 %2)) list1)
                            )) '() (reverse myList)))

;; Task #28
;; Write a function which flattens a sequence.
;; Input: '(1 (3 (5 6) 9 8) 99)
;; Output: '(1 3 5 6 9 8 99)

(fn fun [dataList]
  (loop [xList dataList
        result '()]    
    (cond
      (empty? xList) (reverse result)
      (sequential? (first xList))
        (recur (rest xList) (apply conj result (fun (first xList)) ) )
      :else (recur (rest xList) (conj result (first xList)))
    )
  )
)


;; Task 62
;; Given a side-effect free function f and an initial value x 
;; write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
(fn fun [userFun x]
  (lazy-seq (cons x (fun userFun (userFun x) )))
)

;; Task 135
;; Your friend Joe is always whining about Lisps using the prefix notation for math. 
;; Show him how you could easily write a function that does math using the infix notation. 
;; Is your favorite language that flexible, Joe? Write a function that accepts a variable length mathematical 
;; expression consisting of numbers and the operations +, -, *, and /. Assume a simple calculator that 
;; does not do precedence and instead just calculates left to right.
;;
;; (= 42 (__ 38 + 48 - 2 / 2))

(defn fun [& args]
  (reduce
    (fn [val col]
      ((first col) val (last col))
    )
    0
    (partition 2 (conj args +))
  )
)

;; Task 97
;; Pascal's Triangle
;; (= (map __ (range 1 6))
;;   [     [1]
;;        [1 1]
;;       [1 2 1]
;;      [1 3 3 1]
;;     [1 4 6 4 1]])

(fn fun [n]
  (cond
    (<= n 1) '(1)
    :else  (concat [1] (map #(apply + %) (partition 2 1 (fun (dec n)))) [1])
  )
)

;; Task 95
;; 
;; Write a predicate which checks whether or not a given sequence represents a binary tree. 
;; Each node in the tree must have a value, a left child, and a right child.
;;  (= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
;;     false)
;;  (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
;;      true)

(fn fun [coll]
  (cond
    (not (coll? coll)) (nil? coll)
    (not= (count coll) 3) false
    :else (every? fun (rest coll))
  )
)

;; Task 120
;; Write a function which takes a collection of integers as an argument. 
;; Return the count of how many elements are smaller than the sum of their squared component digits. 
;; For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.
;; (= 8 (__ (range 10)))
;; (= 50 (__ (range 100)))

(fn [col]
  (count (filter true?
    (map #(loop [x % sum 0]
          (if (> x 0)
            (recur (int (/ x 10))
              (+ sum (* (mod x 10) (mod x 10) ))
            )
            (< % sum)
          )          
        )  
    col)
  ))
)

