(ns aoc2020.day2
  (:require [utils :as util]))

(defn parse-password-line [s]
  (let [parsed (re-find #"([0-9]+)-([0-9]+) (.): (.+)" s)]
    [(Integer. (nth parsed 1))
     (Integer. (nth parsed 2))
     (.charAt (nth parsed 3) 0)
     (nth parsed 4)]))

(defn try-parse [s]
  (try
    (parse-password-line s)
    (catch Exception e
      (.printStackTrace e))))

(defn read-password-file []
  (let [file (util/read-file "day2.txt")]
    (map try-parse file)))

(defn is-valid-part1? [[min max ch password]]
  (let [count (count (filter #(= ch %) password))]
    (and (>= count min)
         (<= count max))))

(defn validate-passwords-part1 []
  (count (filterv is-valid-part1? (read-password-file))))

(defn get-char-at [s pos]
  (let [pos (- pos 1)]
    (if (>= pos (count s))
      \u0000
      (char (nth s pos)))))


(defn is-valid-part2? [[pos1 pos2 ch password]]
  (let [ch1 (get-char-at password pos1)
        ch2 (get-char-at password pos2)]
        (= 1 (count (filter #(= ch %) [ch1 ch2])))))

(defn validate-passwords-part2 []
  (count (filterv is-valid-part2? (read-password-file))))

;(validate-passwords-part1)
;(validate-passwords-part2)
