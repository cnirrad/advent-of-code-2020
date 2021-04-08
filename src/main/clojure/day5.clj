(ns day5
  (:require
    [utils :as util]))

(defn do-search [boarding-pass num-seats front back]
  (let [fn (fn [[begin end] next]
              (let [num (+ 1 (- end begin))]
                (condp = next
                  front [begin (- end (/ num 2))]
                  back  [(+ begin (/ num 2)) end]
                  [begin end])))]
    (reduce fn [0 (- num-seats 1)] boarding-pass)))


(defn determine-seat [boarding-pass]
  (let [front-back (.substring boarding-pass 0 7)
        side-side (.substring boarding-pass 7 10)
        row (first (do-search front-back 128 \F \B))
        col (first (do-search side-side 8 \L \R))]
    (+ col (* row 8))))

(defn find-highest-seat []
  (let [boarding-passes (util/read-file "day5.txt")]
    (reduce max 0 (map determine-seat boarding-passes))))

;(find-highest-seat)
;=> 947

(defn determine-my-seat-number []
  (let [boarding-passes (util/read-file "day5.txt")
        seats-taken (sort (map determine-seat boarding-passes))
        first-seat (first seats-taken)
        last-seat (last seats-taken)
        exp-total (reduce + (range first-seat (+ 1 last-seat)))
        act-total (reduce + seats-taken)]
    (println "first seat " first-seat ", last seat " last-seat)
    (println "exp " exp-total " - act " act-total)
    (- exp-total act-total)))

(determine-my-seat-number )