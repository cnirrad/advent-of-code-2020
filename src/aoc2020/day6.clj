(ns aoc2020.day6
  (:require [utils :as util]))

(defn read-file []
  (util/read-file "day6.txt" "\n\n"))

(defn group-to-count-every-answer [grp]
  (->> grp
       (filter #(Character/isLetter %))
       set
       count))

(defn string->set [str]
  (into #{} str))

(defn count-same-answer [grp]
  (let [answers (.split grp "\n")
        ans-sets (map string->set answers)]
    (count
      (reduce clojure.set/intersection ans-sets)
      )))

(defn sum-over-all-groups [fn]
  (let [groups (read-file)]
    (reduce +
      (map fn groups))))

(println "Sum of all answers: " (sum-over-all-groups group-to-count-every-answer))
(println "Sum of same answers: " (sum-over-all-groups count-same-answer))
