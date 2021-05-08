(ns intersection-test
  (:require [clojure.test :refer :all])
  (:require [intersection :refer [intersection]]))


(deftest intersection-test
  (is (= [3 3 5] (intersection [1 2 3 3 4 5 6] [3 3 5]))))