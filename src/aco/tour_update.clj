(in-ns 'aco.core)



;;Compute delta-Tau
;;(1) Use the Route and route offset: [0 1 2 3] [1 2 3 0]
;;(2) Create a hashmap between the 2 vectors with the value being Q/ant trip length
;;;;;;{:01 0.34 :12 0.14 :23 0.54 :03 0.33}
;;(3) Sum the hashmaps for each ant
;;(4) fmap current pheremone table (1-rho)*values
;;(5) sum the updated pheremone table with the hashmaps from step 3


(defn get-offset-route
  "Shift a route exactly one node to the right"
  [path]
  (conj (vec (rest path)) (first path)))


 (defn ant-pher-delta
   "For a single ant's completed tour, return its' pheremone impact on nodes it touched. (Q/tour_length)"
   [n1 n2]
   (let [k (mapv #(hash-key-creator (count (str node-cnt)) % %2) n1 n2)
         v (mapv #(/ Q (dist-map %)) k)]
     (zipmap k v)))


 (defn colony-pher-delta
   [routes]
   (apply merge-with + (map #(ant-pher-delta % (get-offset-route %)) routes)))


 (defn update-pheremones
   [pheremone-table colony-routes rho]
   (merge-with +
               (colony-pher-delta colony-routes)
               (fmap #(* (- 1 rho) %) pheremone-table)))


