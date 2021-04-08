(ns day7
  (:require [utils :as util]))

(defn parse-count [s]
  (if (= "no" s)
    0
    (Integer/parseInt s)))

(defn parse-rule
  "Parses a single line of the rule file and spits out a vector, such as:

    [\"drab blue\" [[5 \"faded teal\"] [4 \"dotted beige\"] [1 \"dull orange\"]]]

    That is, the first element is the bag color of the subject of the rule, followed by a list of
    bags that can fit in the subject bag. Each item in the list is a pair, the first item being
    the number of bags it can fit and the second is the color of the bag.
  "
  [rule]
  (let [words (.split rule " ")
        subject (str (first words) " " (second words))]
    (loop [idx 4
           related-bags []]
      (let [cnt (parse-count (nth words idx))
            bag-name (str (nth words (+ 1 idx)) " " (nth words (+ 2 idx)))
            related-bags (conj related-bags [cnt bag-name])
            next-idx (+ idx 4)]
        (if (<= (count words) next-idx)
          [subject related-bags]
          (recur next-idx
                 related-bags))))))

(defn rule->reverse-bag-map
  "Takes the rules input and creates a map of the bag color => a set
   of bags that is able to hold that bag color"
  [m rule]
  (let [prule (parse-rule rule)
        subject (first prule)]
    (loop [m m
           [[cnt bag] & bags] (second prule)]
      (let [m (if (contains? m bag)
                (update m bag #(conj % subject))
                (assoc m bag #{subject}))]
        (if (not bags)
          m
          (recur m
                 bags))))))


(defn solve-part1 [rule-map]
  (loop [solution-bags #{}
         remaining-to-check (get rule-map "shiny gold")]
    (let [bag (utils/dbg (first remaining-to-check))
          remaining-to-check (utils/dbg (disj remaining-to-check bag))
          solution-bags (conj solution-bags bag)
          remaining-to-check (clojure.set/union remaining-to-check
                                                (set (filter #(not (contains? solution-bags %))
                                                             (get rule-map bag))))]
      ;(println "Working on bag " bag)
      (if (empty? remaining-to-check)
        solution-bags
        (recur solution-bags
               remaining-to-check)))))

(defn part1 []
  (let [rules (util/read-file "day7.txt")
        rule-map (reduce rule->reverse-bag-map {} rules)]
    (count (solve-part1 rule-map))))

(defn rule->bag-map [m rule]
  (let [prule (parse-rule rule)
        subject (first prule)]
    (assoc m subject (second prule))))

(defn part2 []
  (let [rules (util/read-file "day7.txt")
        rule-map (reduce rule->bag-map {} rules)]
    (loop [queue (get rule-map "shiny gold")
           total 0]
      (let [[[cnt bag] & remaining-queue] queue
            req-bags (into [] (map (fn [e] [(* cnt (first e)) (second e)]) (get rule-map bag)))
            remaining-queue (concat remaining-queue req-bags)]
        (println cnt bag " -> " req-bags)
        (if (empty? remaining-queue)
          (+ cnt total)
          (recur remaining-queue
                 (+ cnt total))
          )))))
