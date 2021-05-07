(ns aoc2020.day9
  (:require [utils :as util]))

(defn str->long [s]
  (Long/parseLong s))

(defn calc-all-combos [c]
  (set
    (for [x c
          y c]
      (+ x y))))

(defn is-valid-XMAS-seq [c]
  (contains?
    (calc-all-combos (take 25 c))
    (nth c 25)))

(defn find-invalid [input]
  (loop [cipher input]
    (if-not (is-valid-XMAS-seq cipher)
      (nth cipher 25)
      (recur (drop 1 cipher)))))

(defn read-cipher []
  (map str->long (utils/read-file "day9.txt")))

(defn step1 []
  (find-invalid (read-cipher)))

(defn get-smallest-highest [c]
  (reduce
    (fn [[low high] n] [(min low n) (max high n)])
    [(first c) (first c)]
    c))

(defn step2 []
  (let [input (read-cipher)
        target-num (find-invalid input)]
    (loop [window [(first input)]
           idx 1]
      (let [curr-ttl (reduce + window)]
        (cond
          (> curr-ttl target-num) (recur (vec (drop 1 window))
                                         idx)
          (< curr-ttl target-num) (recur (conj window (nth input idx))
                                         (inc idx))
          :else (apply + (get-smallest-highest window)))))))
