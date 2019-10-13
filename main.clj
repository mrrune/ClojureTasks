;; Source: http://www.4clojure.com
;; Task #33
;; Write a function which replicates each element of a sequence a variable number of times.
;;
;; (= (__ [1 2 3] 2) '(1 1 2 2 3 3))

(fn fun1 [myList n]
  (reduce #(apply conj %1 (loop [i 0 list1 '()]
                            (if (< i n) (recur (inc i) (conj list1 %2)) list1)
                            )) '() (reverse myList)))
