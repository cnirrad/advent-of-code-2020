(ns day1
  (:require [utils :as util]))


(defn remove-first-and-last [v]
  (subvec v 1 (- (count v) 1)))


(defn read-expense-report []
  (let [file (util/read-file "day1.txt")]
      (map #(Integer. %) file)))

(defn find-error2
  "Day 1, part 1 - find two entries that sum to 2020 and multiply them together"
  []
  (let [report (vec (sort (read-expense-report)))
        head (first report)
        tail (peek report)
        report (remove-first-and-last report)]
    (loop [report report
           head head
           tail tail]
      (let [result (condp apply [(+ head tail) 2020]
                     = [:found (* head tail)]
                     > [:recur
                        (pop report)
                        head
                        (peek report)]
                     < [:recor
                        (subvec report 1)
                        (first report)
                        tail])]
        (if (= :found (first result))
          (second result)
          (let [[_ rpt h t] result]
            (if-not (empty? rpt)
              (recur rpt h t))))))))

;(find-error2)

(defn find-third-element [v h t]
  (let [n (- 2020 (+ h t))]
    (if (contains? v n)
      [:found (* h t n)]
      [:recur v (first v) t])))


(defn find-error3
  "Day 1, part 2 - find three entries that sum to 2020 and multiply them together"
  []
  (let [report (vec (read-expense-report))]
    (apply * (first (for [a report
                          b (subvec report 1)
                          c (subvec report 2)
                          :when (= 2020 (+ a b c))] [a b c])))))

(find-error3)