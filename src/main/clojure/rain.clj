;; This is a practice problem from https://www.interviewbit.com/problems/rain-water-trapped/
(ns rain)


;; state is a map that has the following elements
;;    water-map - a map of elevation to how much water could be accumulated, but hasn't been accounted for yet because a right boundry hasn't been found
;;    water-accum - the total amount of water that has been trapped so far
;;    peak - the highest elevation found so far
;;    last-elev - the elevation of the previous iteration

;; On elevation decrease, add 1 to each elevation in the water map from last elevation to max(peak, new-elev)
;; On elevation increase, add and remove from water map for each elevation for the diff in the elevation change

(defn add-to-water-map
  "Helper to add one to every entry in the water-map in the range of start..end"
  [water-map start end]
  (let [added-entries (into {} (for [e (range start end)]
                                 [e 1]))]
    (merge-with + water-map added-entries)))

(defn on-elev-decrease [state new-elev]
  (let [{:keys [peak last-elev water-map]} state
        water-lvl (Math/max peak new-elev)
        new-water-map (add-to-water-map water-map new-elev water-lvl)]
    (assoc-in state [:water-map] new-water-map)))

(defn on-elev-increase [state new-elev]
  (let [{:keys [peak water-map last-elev]} state
        water-lvl (Math/min peak new-elev)
        captured (reduce + (for [e (range last-elev water-lvl)]
                             (get water-map e 0)))
        new-water-map (-> (apply dissoc water-map (range last-elev water-lvl))
                          (add-to-water-map water-lvl peak))]
    (-> state 
        (assoc :water-map new-water-map)
        (update :water-accum + captured))))


(defn capture-iteration [state new-elev]
  (let [state (if (> (:last-elev state) new-elev)
                (on-elev-decrease state new-elev)
                (on-elev-increase state new-elev))
        state (update state :peak #(Math/max % new-elev))
        state (assoc state :last-elev new-elev)]
    (println new-elev state)
    state))


(defn capture [elevations]
  (:water-accum (reduce capture-iteration {:water-accum 0
                                           :water-map {}
                                           :peak 0
                                           :last-elev 0}
                        elevations)))

(comment
  (capture [0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1])
  (capture [0 1 2 0 1 2])
  (capture [4 0 3]) 
  (capture [3 0 4]) 
  )

