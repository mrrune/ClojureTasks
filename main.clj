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
