(ns aco.core-test
  (:require [clojure.test :refer :all]
            [aco.core :refer :all]))



(deftest b-test
  (testing "distance not properly calculated"
    (is (= 5.0 (calc-waypoint-dist [0 0] [3 4])))))

(deftest verify-hashmap-retrieval
  (testing "hash 0-append salt working"
  (is (= 2 (units-count (range 30))))
  (is (= 5 (units-count (range 30000))))
  (is (= 3 (units-count (range 200)))))
  (is (= :13 (hash-key-creator 1 3 1)))
  (is (= :001003 (hash-key-creator 3 3 1)))
  (is (= 55.0 (hash-lookup {:14 55.0} 4 1 )))
  )


