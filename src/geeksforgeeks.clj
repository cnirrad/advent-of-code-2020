(ns geeksforgeeks)


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
  (max-subarray-sum [-1, -2, -3, -4])                       ; => -1
  )



