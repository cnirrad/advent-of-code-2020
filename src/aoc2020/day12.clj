(ns aoc2020.day12
  (:require [utils :as util]))

;; part 1 code

(defn move-dir [[n e d] dir val]
  (condp = dir
    \N [(+ val n) e d]
    \S [(- n val) e d]
    \E [n (+ e val) d]
    \W [n (- e val) d]
    (println "Don't know how to go " dir)))

(def dirs-right [\N \E \S \W])
(def dirs-left (reverse dirs-right))

(defn abs [v] (Math/abs v))

(defn rotate [[n e d] degrees]
  (let [clicks (inc (/ (abs degrees) 90))
        dirs (if (> degrees 0) dirs-right dirs-left)
        new-dir (last (take (abs clicks)
                            (drop-while #(not (= d %))
                                        (cycle dirs))))]
    [n e new-dir]))

(defn interpret-instruction [coord inst]
  (let [dir (last coord)
        val (Integer/parseInt (re-find #"\d+" inst))
        dir-inst (first inst)]
    (condp = dir-inst
      \L (rotate coord (* -1 val))
      \R (rotate coord val)
      \F (move-dir coord dir val)
      (move-dir coord dir-inst val))))


(defn find-manhatten-distance-travelled []
  (let [instructions (util/read-file "day12.txt")
        end (reduce (partial util/wrap-println interpret-instruction) [0 0 \E] instructions)]
    (println end)
    (+ (abs (first end))
       (abs (second end)))))


(comment
  (find-manhatten-distance-travelled)) ; => 1032
