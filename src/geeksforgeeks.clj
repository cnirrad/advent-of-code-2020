(ns geeksforgeeks
  (:use [clojure.set :refer [difference]]))


;; https://practice.geeksforgeeks.org/problems/subarray-with-given-sum-1587115621/1

(defn find-subarray-with-sum [arr total]
  (loop [start-idx 0
         end-idx 1]
    (if (= start-idx end-idx)
      [-1 -1]
      (let [sub-arr-total (reduce + (subvec arr start-idx end-idx))]
        (println start-idx ", " end-idx ": "  (subvec arr start-idx end-idx) " = " sub-arr-total)
        (cond
          (= sub-arr-total total) [(inc start-idx) end-idx]
          (> total sub-arr-total) (recur start-idx (inc end-idx))
          (< total sub-arr-total) (recur (inc start-idx) (dec end-idx)))))))


(comment
  (find-subarray-with-sum [1,2,3,4,5,6,7,8,9,10] 15)
  (find-subarray-with-sum [1 2 3 7 5] 12))



;; https://practice.geeksforgeeks.org/problems/count-the-triplets4615/1
(defn count-triplets [arr]
  (let [sums (into #{} (for [x arr
                             y arr
                             :when (not (= x y))]
                         (+ x y)))]
    (count (filter #(contains? sums %) arr))))

(comment
  (count-triplets [2 3 4])
  (count-triplets [1, 5, 3, 2]))


;; https://practice.geeksforgeeks.org/problems/kadanes-algorithm-1587115620/1

(defn max-reducer [[local-max global-max] val]
  (let [local (Math/max val (+ local-max val))]
    [local (Math/max local global-max)]))

(defn max-subarray-sum [arr]
  (second (reduce max-reducer [0 Integer/MIN_VALUE] arr)))

(comment
  (max-subarray-sum [1, 2, 3, -2, 5])                       ; => 9)
  (max-subarray-sum [-1, -2, -3, -4]))                       ; => -1



;; find all factors of a number

(defn factors-of [n]
  (loop [factors #{1 n}
         divisor 2]
    (println "n = " n ", divisor = " divisor ", factors = " factors)
    (if (< (Math/sqrt n) divisor)
      factors
      (if (= 0 (mod n divisor))
        (recur (conj factors divisor (/ n divisor))
               (inc divisor))
        (recur factors (inc divisor))))))

(comment
  (factors-of 15)
  (factors-of 12))

;; https://www.geeksforgeeks.org/ugly-numbers/
(defn is-ugly? [n]
  (let [max-div (fn [num denom]
                  (loop [a num]
                    (if (= 0 (mod a denom))
                      (recur (/ a denom))
                      a)))
        no (max-div n 2)
        no (max-div no 3)
        no (max-div no 5)]
    (= 1 no)))

(defn get-nth-ugly-num [n]
  (loop [cnt 1
         i 1]
    (if (= cnt n)
      i
      (recur (if (is-ugly? (inc i))
               (inc cnt)
               cnt)
             (inc i)))))

(comment
  (is-ugly? 5832)
  (get-nth-ugly-num 150))


;; https://www.geeksforgeeks.org/coin-change-dp-7/

(defn find-change [change-for coins-available]
  (loop [solutions []
         current [(first coins-available)]
         stack (into [] (for [c (rest coins-available)] [c]))]
    (println "current=" current "stack=" stack "solutions=" solutions)
    (if (and (empty? current) (empty? stack))
      solutions
      (let [total (reduce + current)
            solutions (if (= total change-for) (conj solutions current) solutions)
            next-coins (filterv #(and (<= % (- change-for total))
                                      (>= % (peek current))) coins-available)
            new-stack (if (empty? next-coins)
                        stack
                        (into stack (for [c next-coins]
                                      (conj current c))))
            current (peek new-stack)]
        (println "total=" total "solutions=" solutions "next-coins=" next-coins "new-stack=" new-stack "current=" current)
        (if (empty? new-stack)
          solutions
          (recur solutions current (pop new-stack)))))))


(comment
  (find-change 4 [1 2 3])
  (find-change 10 [2 5 3 6]))



;; https://www.geeksforgeeks.org/longest-common-subsequence-dp-4/

(defn lcs-recursive [s1 s2 idx1 idx2]
  (if (or (= idx1 0)
          (= idx2 0))
    0
    (if (= (nth s1 (dec idx1))
           (nth s2 (dec idx2)))
      (+ 1 (lcs-recursive s1 s2 (dec idx1) (dec idx2)))
      (Math/max (lcs-recursive s1 s2 idx1 (dec idx2))
                (lcs-recursive s1 s2 (dec idx1) idx2)))))

(comment
  (lcs-recursive "ABCDGH" "AEDFHR" 6 6)
  (lcs-recursive "AGGTAB" "GXTXAYB" 6 7))

;; https://www.geeksforgeeks.org/count-ways-reach-nth-stair/

(defn stair-counting [n]
  (if (<= n 2)
    n
    (+ (stair-counting (- n 2))
       (stair-counting (dec n)))))

(defn stair-counting-dp [n]
  (loop [solutions [0 1 2]
         stair 3]
    (if (> stair n)
      (peek solutions)
      (recur (conj solutions (+ (nth solutions (- stair 1))
                                (nth solutions (- stair 2))))
             (inc stair)))))

(comment
  (stair-counting-dp 4)
  (stair-counting 4))
