(in-ns 'aco.core)

(defn hash-key-creator
  "Create a hash-key given two nodes"
  [node-total node-a node-b]
  (let [key-padding (str "%0" node-total "d")]
    (keyword(reduce str (map #(format key-padding %) (sort [node-a node-b]))))))

(defn units-count
  "Count the order of magnitude for a matrix - used to build hashmap ids"
  [matrix]
  (count (str (count matrix))))

(defn hash-lookup
  "Query the hash-map to find the hashvalue between 2 nodes"
  ([hash-table node-a node-b]
   (hash-table (hash-key-creator (units-count hash-table) node-a node-b)))
  ([hash-table node-a node-b units]
   (hash-table (hash-key-creator (count (str units)) node-a node-b))))

(defn dist-hash-lookup
  "Lookup if distance table and node-cnt parameters exist"
  [node-a node-b]
  (hash-lookup dist-map node-a node-b node-cnt))

(defn pdf-hash-lookup
  "Lookup if pheremone table and node-cnt parameters exist"
  [node-a node-b]
  (hash-lookup pher-map node-a node-b node-cnt))

(defn pdf-hash-lookup
  "Lookup probability distribution function if pheremone table and node-cnt parameters exist"
  [node-a node-b]
  (hash-lookup pdf-map node-a node-b node-cnt))

;;(defn pdf-lookup
;;  "Query the distance hash-map to find the distance between 2 nodes"
;;  ([pdf-matrix node-a node-b]
;;   (let [key-padding (str "%0" (count (str node-cnt)) "d")]
;;    (pdf-matrix (keyword(reduce str (map #(format key-padding %) (sort [node-a node-b])))))))
;;  ([node-a node-b]
;;   (let [key-padding (str "%0" (count (str node-cnt)) "d")]
;;     (pdf-map (keyword(reduce str (map #(format key-padding %) (sort [node-a node-b]))))))))

(defn remove-node
  "Remove a selected node from a vector of remaining nodes"
  [node remaining-nodes]
  (let [node-index (.indexOf remaining-nodes node)]
    (reduce conj (subvec remaining-nodes 0 node-index) (subvec remaining-nodes (inc node-index) (count remaining-nodes)))))

(defn compute-route-dist
  "Given a route, determine the distance to do a complete loop*"
  [path]
  (let [path-offset (conj (vec (rest path)) (first path))]
    (reduce + (mapv #(dist-hash-lookup % %2) path path-offset))))

(defn compute-colony-dist
  [single-ant-route]
  "Create vector of tour distance for each ant"
  (mapv #(compute-route-dist %) single-ant-route))






;;(1) Create path starting point
;;(def path (atom (conj [] (rand-int node-cnt))))
;;@path

;;(2) Create vector of available nodes
;;(def remaining-nodes (atom (vec(range node-cnt))))

;;(reset! remaining-nodes (remove-node (last @path) @remaining-nodes))


;;(3) Compute likelyhood vector of selecting each of the remaining nodes (not normalized)
;;(def weighting (mapv #(pdf-lookup (last @path) %) @remaining-nodes))
;;weighting


;;(4) Compute weighting threshold
;;(def threshold (rand (reduce + weighting)))
;;threshold

;;(5) Identify next node (via identifying index of weight that first exceeds the weighting threshold)
;;(defn select-node-index
;;  [weights thresh nodes]
;;  (nodes (.indexOf (reductions + weights) (first (filter #(< thresh %) (reductions + weights))))))
;;(def selection (select-node-index weighting threshold @remaining-nodes))

;;(6) Add selected node to path & update remaining-nodes
;;(swap! path conj selection)
;;(reset! remaining-nodes (remove-node (last @path) @remaining-nodes))
;;@path
;;@remaining-nodes

;;;;;;;;;;
;;PUT ABOVE STEPS INTO A SINGLE FUNCTION

;;Compute the path taken by the ant



(defn select-node-index
  "Identify next node (via identifying index of weight that first exceeds the weighting threshold)"
  [weights thresh nodes]
  (nodes (.indexOf (reductions + weights) (first (filter #(< thresh %) (reductions + weights))))))

;;Dependencies: pdf-lookup
(defn compute-path
  "Return the ant's path when provided the probabalistic distribution function lookup"
  ([path open-nodes pdf-map]
  (if (empty? open-nodes)
    path
    (let [weight-vector (mapv #(hash-lookup pdf-map (last path) % node-cnt) open-nodes)
        threshold (rand (reduce + weight-vector))
        selected-node (select-node-index weight-vector threshold open-nodes)]
      (compute-path (conj path selected-node) (remove-node selected-node open-nodes) pdf-map ))))
  ([nodes pdf-map]
   (let [path []
         open-nodes (vec(range nodes))
         starting-point (rand-int nodes)]
   (compute-path (conj path starting-point) (remove-node starting-point open-nodes) pdf-map))))

(defn compute-colony-path
  "Compute the route for all the ants in the colony"
  [pdf-table node-cnt ant-cnt]
   (vec (repeatedly ant-cnt #(compute-path node-cnt pdf-table))))

