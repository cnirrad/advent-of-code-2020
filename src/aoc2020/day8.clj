(ns aoc2020.day8
  (:require [utils :as util]))

(defn raw-instruction->instruction [raw-instr]
  (let [[op arg] (.split raw-instr " ")]
    [(keyword op) (Integer/parseInt arg)]))

(defn parse-instructions []
  (let [input (util/read-file "day8.txt")]
    (mapv raw-instruction->instruction input)))

(defn run-instructions [instructions]
  (let [num-instructions (count instructions)]
    (loop [cur-instr 0
           accum 0
           visited #{}]
;      (println cur-instr ":" (nth instructions cur-instr))
      (if (or (contains? visited cur-instr)
              (>= cur-instr num-instructions))
        {:accum accum
         :looped (contains? visited cur-instr)
         :visited visited}
        (condp = (first (nth instructions cur-instr))
          :acc (recur (inc cur-instr)
                      (+ accum (second (nth instructions cur-instr)))
                      (conj visited cur-instr))
          :jmp (recur (+ cur-instr (second (nth instructions cur-instr)))
                      accum
                      (conj visited cur-instr))
          (recur (inc cur-instr)
                 accum
                 (conj visited cur-instr)))))))

(defn part1 []
  (run-instructions (parse-instructions)))

(defn flip-instruction [instructions idx]
  (let [[op arg] (nth instructions idx)]
    (if (= op :nop)
      (update-in instructions [idx 0] :jmp)
      (update-in instructions [idx 0] :nop))))

(defn part2 []
  (let [instructions (parse-instructions)
        initial-result (run-instructions instructions)
        change-candidates (filter #(not (= :acc %)) (:visited initial-result))]
    (first (for [idx change-candidates
          :let [new-inst (flip-instruction instructions idx)
                results (run-instructions new-inst)]
          :when (= (:looped results) false)]
      (:accum results)))))