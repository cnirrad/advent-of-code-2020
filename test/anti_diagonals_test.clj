(ns anti-diagonals-test
  (:require [clojure.test :refer :all])
  (:require [anti-diagonals :refer [get-anti-diagonal find-anti-diagonals]]))

(def matrix [[1 2 3]
             [4 5 6]
             [7 8 9]])

(deftest get-anti-diagonal-test
  (is (= [2 4] (get-anti-diagonal matrix 1 0)))
  (is (= [6 8] (get-anti-diagonal matrix 2 1))))


(deftest find-antidiagonals-test
  (is (= [
          [1]
          [2 4]
          [3 5 7]
          [6 8]
          [9]
          ]
         (find-anti-diagonals matrix))))