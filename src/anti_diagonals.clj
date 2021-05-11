(ns anti-diagonals)

; https://www.interviewbit.com/problems/anti-diagonals/

(defn get-anti-diagonal [matrix col row]
  (let [size (count matrix)]
    (loop [col col
           row row
           vals []]
      (if (or (< col 0) (>= row size))
        vals
        (recur (dec col)
               (inc row)
               (conj vals (get-in matrix [row col])))))))

(defn find-anti-diagonals
  "Given an NxN square matrix, return an array of its anti-diagonals.

  Matrix is a vector of vectors, such as
  [[1 2 3]
   [4 5 6]
   [7 8 9]]

  Output would be:
   [
     [1]
     [2 4]
     [3 5 7]
     [6 8]
     [9]
   ]
  "
  [matrix]
  (let [size (count matrix)]
    (loop [col 0
           row 0
           results []]
      (let [results (conj results (get-anti-diagonal matrix (min (dec size) col) row))
            col (if (< col size) (inc col) col)
            row (if (= col size) (inc row) row)]
        (comment println "row = " row ", col = " col ", results = " results)
        (if (and (= col size) (= row size))
          results
          (recur col
                 row
                 results))))))
