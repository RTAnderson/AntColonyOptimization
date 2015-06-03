(in-ns 'aco.core)

(defn create-waypoints
  "Initialize a vector of waypoints to be traversed by the ants"
[node-cnt max-val]
(let [random-point (fn[](rand max-val))
      random-waypoint (fn[](vec (repeatedly 2 random-point)))
      random-waypoints (vec(repeatedly node-cnt random-waypoint))]
  random-waypoints))



(defn calc-waypoint-dist
  [waypt-A waypt-B]
  (let [y (- (second waypt-B) (second waypt-A)) x (- (first waypt-B) (first waypt-A))]
    (Math/sqrt (+ (* x x) (* y y)))))




(defn create-distance-lookup-table
  "Create a hash-map of distance between all possible nodes"
  [waypts]
  (let [unstructured-edges (loop [n (dec (count waypts)) m 0 s 1 t (inc n) alpha () beta ()]
                             (if (zero? n)
                               [alpha beta]
                               (recur (dec n)(inc m)(inc s) t
                                      (conj alpha (repeat n m))
                                      (conj beta (range s t)))))
        edges-A (flatten(first unstructured-edges))
        edges-B (flatten(second unstructured-edges))
        key-padding (str "%0" (count (str (count waypts))) "d")
        lookup-keys (map (fn[a b](keyword(str (format key-padding a) (format key-padding b)))) edges-A edges-B)
        lookup-values (map #(calc-waypoint-dist (nth waypts %) (nth waypts %2)) edges-A edges-B)]
    (zipmap lookup-keys lookup-values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(defn create-new-route
;;  [waypoints]
;;  (let [selected-nodes (rand-int (count waypoints))]
;;  selected-nodes))




(defn create-pheremone-lookup-table
  "Create a hash-map of distance between all possible nodes"
  [waypts]
  (let [unstructured-edges (loop [n (dec (count waypts)) m 0 s 1 t (inc n) alpha () beta ()]
                             (if (zero? n)
                               [alpha beta]
                               (recur (dec n)(inc m)(inc s) t
                                      (conj alpha (repeat n m))
                                      (conj beta (range s t)))))
        edges-A (flatten(first unstructured-edges))
        edges-B (flatten(second unstructured-edges))
        key-padding (str "%0" (count (str (count waypts))) "d")
        lookup-keys (map (fn[a b](keyword(str (format key-padding a) (format key-padding b)))) edges-A edges-B)
        lookup-values (repeat (count lookup-keys) 1.0)]
    (zipmap lookup-keys lookup-values)))

(defn create-pdf-lookup-table
  "Creates the numerator of the node probability distribution function (pdf)"
  [distance-matrix pheremone-matrix alpha-coeff beta-coeff]
  (let [lambda-top (fmap #(Math/pow %1 alpha) pheremone-matrix)
        rho-top (fmap #(Math/pow (/ 1 %1) beta) distance-matrix)]
    (apply merge-with * [rho-top lambda-top])))
