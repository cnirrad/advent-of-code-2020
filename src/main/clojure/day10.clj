(ns day10
  (:require [utils :as util]))

(defn str->num [s]
  (Integer/parseInt s))

(def adaptors' (vec (conj (sort (map str->num (util/read-file "day10-example2.txt"))) 0)))
(def device-joltage (+ 3 (last adaptors')))
(def adaptors (conj adaptors' device-joltage))
(def adaptor-count (count adaptors))

(defrecord AdaptorState [joltage jolt-diff1 jolt-diff3 adaptor-idx])

(defn ->AdaptorState []
  (AdaptorState. 0 0 0 0))

(defn inc-jolt-diff1 [s]
  (update-in s [:jolt-diff1] inc))

(defn inc-jolt-diff3 [s]
  (update-in s [:jolt-diff3] inc))

(defn set-joltage [s jolt]
  (assoc s :joltage jolt))

(defn set-adaptor-idx [s idx]
  (assoc s :adaptor-idx idx))




(defn find-adaptor-index [adaptor start]
   (+ start
      (loop [idx start]
        (if (or (>= idx adaptor-count)
                (< adaptor (nth adaptors idx)))
          (- idx start)
          (recur (inc idx))))))

(defn update-adaptor-state [st adaptor]
  ;(println "current jolt = " (:joltage st) ", next adaptor = " adaptor)
  (when (or (> (:joltage st) adaptor)
            (> adaptor (+ 3 (:joltage st))))
    (throw (IllegalStateException. (pr-str st " cannot fit " adaptor))))

  (let [diff (- adaptor (:joltage st))
        idx (find-adaptor-index adaptor (:adaptor-idx st))
        st (-> st
               (set-joltage adaptor)
               (set-adaptor-idx idx))]
    ;(pr st)
    (condp = diff
      1 (inc-jolt-diff1 st)
      3 (inc-jolt-diff3 st)
      st)))

(defn step1 []
  (let [st (reduce update-adaptor-state (->AdaptorState) adaptors)]
    (println "Final state: " st)
    (* (:jolt-diff1 st)
       (+ 1 (:jolt-diff3 st)))))

(defn get-candidate-adaptors [st]
  (let [max-jolt (+ (:joltage st) 3)]
    (loop [candidates []
           idx (:adaptor-idx st)]
      (if (> (nth adaptors idx) max-jolt)
        candidates
        (recur (conj candidates (nth adaptors idx))
                     (inc idx))))))

