(ns rain-test
  (:require [clojure.test :refer :all])
  (:require [rain :refer [capture]]))

(deftest capture-test
  (is (= 1 (capture [1 0 1])))
  (is (= 2 (capture [5 0 2])))
  (is (= 6 (capture [0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1]))))

