(ns aoc2020.day11
  (:require [utils :as util]))


(defn get-seat-at [seats [row col]]
  (if (or (< row 0)
          (< col 0)
          (>= row (count seats))
          (>= col (count (nth seats row))))
    \E
    (nth (nth seats row) col)))

(defn is-filled? [seats coord]
  (= \# (get-seat-at seats coord)))

(defn count-adj-filled-seats [seats row col]
  (reduce (fn [cnt seat]
            (if (is-filled? seats seat)
              (inc cnt)
              cnt))
          0
          [
           [(dec row) (dec col)]
           [(dec row) col]
           [(dec row) (inc col)]
           [row (dec col)]
           [row (inc col)]
           [(inc row) (dec col)]
           [(inc row) col]
           [(inc row) (inc col)]
           ]
          ))


(defn map-seating-chart [func seats]
  (map-indexed
   (fn [row-num row]
     ;(println "row " row-num ": " row)
     (map-indexed (fn [col-num col]
                    ;(println "col " col-num ": " col)
                    (func row-num col-num col))
                  row))
     seats))

(defn determine-seat [seats row col seat]
  (if (= \. seat)
    seat
    (let [adj-filled (count-adj-filled-seats seats row col)]
      ;(println "seat at " row "," col "=" seat " and has " adj-filled " filled.")
      (if (is-filled? seats [row col])
        (if (>= adj-filled 4)
          \L
          \#)
        (if (= adj-filled 0)
          \#
          \L)))))

(defn pr-seat-chart [seats]
  (doseq [row seats]
    (println (apply str row))))

(defn count-filled-row [row]
  (reduce #(if (= \# %2) (inc %1) %1) 0 row))

(defn count-filled [seats]
  (reduce + (map count-filled-row seats)))

(defn step1 []
  (loop [seats (map vec (util/read-file "day11.txt"))
         loop-cnt 0]
    (if (> loop-cnt 5000)
      (println "Too many iterations")
      (let [new-seats (map-seating-chart #(determine-seat seats %1 %2 %3) seats)]
        (assert (= (count new-seats) (count seats)))
        (assert (= (count (second new-seats)) (count (second seats))))
        (if (= seats new-seats)
          (count-filled seats)
          (recur new-seats
                (inc loop-cnt)))))))
