(ns day3
  (:require [utils :as util]))


(defn tree? [map x y]
  (= \# (nth (nth map y) x)))

(defn count-trees [slope_x slope_y]
  (let [map (util/read-file "day3.txt")
        width (count (first map))
        height (count map)]
    (loop [x 0
           y 0
           num_trees 0]
      (let [new_x (mod (+ x slope_x) width)
            new_y (+ y slope_y)]
        (if (>= new_y height)
          num_trees
          (recur new_x
                 new_y
                 (if (tree? map new_x new_y)
                   (inc num_trees)
                   num_trees)))))))

(*
  (count-trees 1 1)
  (count-trees 3 1)
  (count-trees 5 1)
  (count-trees 7 1)
  (count-trees 1 2))
