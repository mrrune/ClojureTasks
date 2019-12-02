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

;; Task 147
;; Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors, 
;; where each next one is constructed from the previous following the rules used in Pascal's Triangle. 
;; For example, for [3 1 2], the next row is [3 4 3 2].
(fn fun [[x :as vec]]
    (cons vec
        (lazy-seq
          (fun (into [] (concat [x] (map #(apply +' %) (partition 2 1 [0] vec)))))
        )
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

;; Task 96
;; Let us define a binary tree as "symmetric"
;; if the left half of the tree is the mirror image of the right half of the tree.
;; Write a predicate to determine whether or not a given binary tree is symmetric.
;; (see To Tree, or not to Tree for a reminder on the tree representation we're using).
;; (= (__ '(:a (:b nil nil) (:b nil nil))) true)
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
;;   true)
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
;;   false)
(fn fun [[x y z :as col]]
  (cond
    (and (coll? y) (coll? z)) 
      (and
        (= (first y) (first z))
        (fun (list :x (second y) (last z)))
        (fun (list :x (second z) (last y)))
      )
    :else (and (= 3 (count col)) (= y z))
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

;; Task 58
;; Write a function which allows you to create function compositions.
;; The parameter list should take a variable number of functions, 
;; and create a function that applies them from right-to-left.
;; (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
;; (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

(fn [& fns]
  (fn [& col]
    (loop [functions (rest (reverse fns))
            result (apply (last fns) col)]
      (if (empty? functions) 
        result
        (recur (rest functions) ((first functions) result))
      )
    )
  )   
)

;; Task 115
;; A balanced number is one whose component digits have the same sum on the left and right halves of the number.
;; Write a function which accepts an integer n, and returns true iff n is balanced.
;; (= true (__ 89089))
(fn [x]
    (let [k (int (/ (count (str x)) 2)) ]
      (= 
        (reduce
          #(merge-with + % {%2 1})
          {}
          (take k (str x))  
        )
        (reduce
          #(merge-with + % {%2 1})
          {}
          (take-last k (str x))  
        )
      )
    )
)

;; Task 110
;; Write a function that returns a lazy sequence of "pronunciations" of a sequence of numbers.
;; A pronunciation of each element in the sequence consists of the number of repeating identical numbers 
;; and the number itself. For example, [1 1] is pronounced as [2 1] ("two ones"), 
;; which in turn is pronounced as [1 2 1 1] ("one two, one one").

;; Your function should accept an initial sequence of numbers, and return an infinite lazy sequence of pronunciations, 
;; each element being a pronunciation of the previous element.

(fn fun [col]
  (let [x (reduce
            #(conj % (count %2) (first %2))
            []
            (partition-by identity col)
          )
        ]
    (cons
      x
      (lazy-seq (fun x))
    )
  )
)

;; Task 108
;; Given any number of sequences, each sorted from smallest to largest, find the smallest single number
;; which appears in all of the sequences. The sequences may be infinite, so be careful to search lazily.
;; (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))
;;(= 64 (__ (map #(* % % %) (range)) ;; perfect cubes
;;        (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
;;         (iterate inc 20))) ;; at least as large as 20
(fn [& args]
  (loop [counters (take (count args) (repeat 0))]
      (let [elements (map
                    #(first (drop % %2))
                    counters
                    args
                  )
          maxElem (apply max elements)
          minElem (apply min elements)
      ]

        (if (= minElem maxElem) 
          minElem
          (recur (map
                    #(if (not= % maxElem) (inc %2) %2 )
                    elements
                    counters
                  )
          )))))
