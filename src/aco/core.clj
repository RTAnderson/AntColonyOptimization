(ns aco.core
  (:use [clojure.algo.generic.functor :only (fmap)])
  (:gen-class :main true))


;;GLOBAL CONSTANTS
(def node-cnt 49)
(def ant-cnt 30)
(def alpha 1)
(def beta 1.2)
(def evap-rate 0.1)
(def iteration-cnt 10)
(def Q 1)
(load "initialize")
(def waypoints (create-waypoints node-cnt 100))
(def dist-map (create-distance-lookup-table waypoints))
(def optimal-tour-dist (atom []))


;;TOUR PARAMETERS (CONSTANT FOR ALL ANTS DURATION OF TOUR)
(def pher-map (atom (create-pheremone-lookup-table waypoints)))
(def pdf-map (atom (create-pdf-lookup-table dist-map @pher-map alpha beta)))
(load "calculate")
(load "tour_update")

;;ANT PARAMETERS (PARAMETERS UNIQUE TO EACH ANT; CHANGES EACH TOUR)
(def colony-routes (atom (compute-colony-path @pdf-map node-cnt ant-cnt)))

(def colony-tour-length (compute-colony-dist @colony-routes))


(defn execute
  [iterations]
  (dotimes [n iterations]
    (reset! pdf-map (create-pdf-lookup-table dist-map @pher-map alpha beta))
    (reset! colony-routes (compute-colony-path @pdf-map node-cnt ant-cnt))
    (swap! optimal-tour-dist conj (reduce min (compute-colony-dist @colony-routes)))
    (reset! pher-map (update-pheremones @pher-map @colony-routes evap-rate)))
  @optimal-tour-dist)

(def output (execute 10))
output
(reduce min output)




(load "tour_update")

