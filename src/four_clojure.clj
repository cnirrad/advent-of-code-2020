(ns four-clojure
  (:require [clojure.test :refer :all]))


;; https://www.4clojure.com/problem/19
;; Write a function which returns the last element in a sequence.
(def my-last
  (fn [arr]
    (first (drop (dec (count arr)) arr))))

(deftest test-19
  (is (= (my-last [1 2 3 4 5]) 5))
  (is (= (my-last '(5 4 3)) 3))
  (is (= (my-last ["b" "c" "d"]) "d")))


;; https://www.4clojure.com/problem/20
;; Write a function which returns the second to last element from a sequence.
(def penultimate
  (fn [c]
    (last (take (dec (count c)) c))))

(deftest test-20
  (is (= (penultimate (list 1 2 3 4 5)) 4))
  (is (= (penultimate ["a" "b" "c"]) "b"))
  (is (= (penultimate [[1 2] [3 4]]) [1 2])))


;; https://www.4clojure.com/problem/21
;; Write a function which returns the Nth element from a sequence.
(def my-nth
  (fn [c i]
    (first (drop i c))))

(deftest test-21
  (is (= (my-nth '(4 5 6 7) 2) 6))
  (is (= (my-nth [:a :b :c] 0) :a))
  (is (= (my-nth [1 2 3 4] 1) 2))
  (is (= (my-nth '([1 2] [3 4] [5 6]) 2) [5 6])))

;; https://www.4clojure.com/problem/22
;; Write a function which returns the total number of elements in a sequence.

(def my-count
  (fn [c]
    (reduce (fn [acc _] (inc acc)) 0 c)))

(deftest test-22
  (is (= (my-count '(1 2 3 3 1)) 5))
  (is (= (my-count "Hello World") 11))
  (is (= (my-count [[1 2] [3 4] [5 6]]) 3))
  (is (= (my-count '(13)) 1))
  (is (= (my-count '(:a :b :c)) 3)))


;; https://www.4clojure.com/problem/23
;; Reverse a sequence

(def my-reverse
  (fn [c]
    (reduce #(cons %2 %1) (empty c) c)))

(deftest test-23
  (is (= (my-reverse [1 2 3 4 5]) [5 4 3 2 1]))
  (is (= (my-reverse (sorted-set 5 7 2 7)) '(7 5 2)))
  (is (= (my-reverse [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

;; https://www.4clojure.com/problem/24
;; Sum a list of numbers
(def sum
  (fn [c]
    (reduce + c)))

(deftest test-24
  (is (= (sum [1 2 3]) 6))
  (is (= (sum (list 0 -2 5 5)) 8))
  (is (= (sum #{4 2 1}) 7))
  (is (= (sum '(0 0 -1)) -1))
  (is (= (sum '(1 10 3)) 14)))


;; https://www.4clojure.com/problem/25
;; Find the odd numbers
(def find-odd
  (fn [c]
    (filter #(= (mod % 2) 1) c)))


(deftest test-25
  (is (= (find-odd #{1 2 3 4 5}) '(1 3 5)))
  (is (= (find-odd [4 2 1 6]) '(1)))
  (is (= (find-odd [2 2 4 6]) '()))
  (is (= (find-odd [1 1 1 3]) '(1 1 1 3))))


;; https://www.4clojure.com/problem/26
;; Fib
(def fib-recursive
  (fn [n]
    (condp = n
      0 0
      1 1
      2 1
      (+ (fib-recursive (dec n))
         (fib-recursive (- n 2))))))

(def fib
  (fn [n]
    (loop [nums [1 1]
           i 3]
      (let [nums (conj nums (+ (nth nums (- i 3))
                               (nth nums (- i 2))))]
        (if (> n i)
          (recur nums (inc i))
          (seq nums))))))

(deftest test-26
  (is (= (fib 3) '(1 1 2)))
  (is (= (fib 6) '(1 1 2 3 5 8)))
  (is (= (fib 8) '(1 1 2 3 5 8 13 21))))

;; https://www.4clojure.com/problem/27
;; Write a function which returns true if the given sequence is a palindrome.

(def is-palindrome?
  (fn [c]
    (= (reverse c) (seq c))))

(deftest test-27
  (is (false? (is-palindrome? '(1 2 3 4 5))))
  (is (true? (is-palindrome? "racecar")))
  (is (true? (is-palindrome? [:foo :bar :foo])))
  (is (true? (is-palindrome? '(1 1 3 3 1 1))))
  (is (false? (is-palindrome? '(:a :b :c)))))

;; https://www.4clojure.com/problem/28
;; Flatten a sequence

(def my-flatten
  (fn [c]
    (seq (reduce (fn flatten-it [acc v]
                   (if (coll? v)
                     (reduce flatten-it acc v)
                     (conj acc v))) [] c))))


(deftest test-28
  (is (= (my-flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (is (= (my-flatten ["a" ["b"] "c"]) '("a" "b" "c")))
  (is (= (my-flatten '((((:a))))) '(:a))))

;; https://www.4clojure.com/problem/29
;; Return a string containing only the capital letters

(def get-caps
  (fn [s]
    (apply str (filter #(and (>= (int %) (int \A))
                             (<= (int %) (int \Z))) s))))

(deftest test-29
  (is (= (get-caps "HeLlO, WoRlD!") "HLOWRD"))
  (is (empty? (get-caps "nothing")))
  (is (= (get-caps "$#A(*&987Zf") "AZ")))

;; https://www.4clojure.com/problem/30
;; Write a function which removes consecutive duplicates from a sequence.

(def my-dedupe
  (fn [c]
    (loop [result []
           c c
           prev nil]
      (if-let [curr (first c)]
        (if (= curr prev)
          (recur result
                 (drop 1 c)
                 (first c))
          (recur (conj result (first c))
                 (drop 1 c)
                 (first c)))
        result))))

(deftest test-30
  (is (= (apply str (my-dedupe "Leeeeeerrroyyy")) "Leroy"))
  (is (= (my-dedupe [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
  (is (= (my-dedupe [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

