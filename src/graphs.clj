(ns graphs)

(def graph {:A [:B :C]
            :B [:A :X]
            :X [:B :Y]
            :Y [:X]
            :C [:A :D]
            :D [:C :E :F]
            :E [:D :G]
            :F [:D :G]
            :G [:E :F]})

(defn get-neighbors [graph v]
  (get graph v))

(defn visited? [visited v]
  (some #(= v %) visited))

(def unvisited?
  (complement visited?))

(defn search-impl [graph v collection]
  (loop [stack (conj collection v)
         visited []]
    (if (empty? stack)
      visited
      (let [v (peek stack)
            neighbors (get-neighbors graph v)
            not-visited (filterv #(unvisited? visited %) neighbors)
            stack (apply conj (pop stack) not-visited)
            visited (if (unvisited? visited v)
                      (conj visited v)
                      visited)]
        (println "stack=" (vec stack) "visited=" visited)
        (recur stack visited)))))

(defn dfs
  "Depth First Search"
  [graph v]
  (search-impl graph v []))

(defn bfs
  "Breadth First Search"
  [graph v]
  (search-impl graph v (clojure.lang.PersistentQueue/EMPTY)))

(comment
  (dfs graph :A)
  (bfs graph :A)
  )