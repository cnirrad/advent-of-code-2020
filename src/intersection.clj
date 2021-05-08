(ns intersection)

; Find the intersection of two sorted arrays.
; https://www.interviewbit.com/problems/intersection-of-sorted-arrays/

(defn intersection [arr1 arr2]
  (loop [a1 arr1
         a2 arr2
         output []]
    (if (or (empty? a1) (empty? a2))
      output
      (let [head1 (first a1)
            head2 (first a2)]
        (comment println "comparing " head1 " to " head2)
        (cond
          (= head1 head2) (recur (rest a1) (rest a2) (conj output head1))
          (< head1 head2) (recur (rest a1) a2 output)
          (> head1 head2) (recur a1 (rest a2) output))))))
