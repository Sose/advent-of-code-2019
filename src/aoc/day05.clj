(ns aoc.day05
  (:require [aoc.core :as core]
            [clojure.string :as s]
            [aoc.intcode :refer :all]))

(def input-state (apply vector (map read-string (s/split (core/input-for 05) #","))))
;;(def input-state [1,9,10,3,2,3,11,0,99,30,40,50])
;;(def input-state [1002,4,3,4,33])
;;(def input-state [3,0,4,0,99])
;;(def input-state [3,9,8,9,10,9,4,9,99,-1,8])

(def computer (make-computer input-state))

(defn part1 []
  (let [outputs (:outputs (run-computer computer [1]))]
    (last outputs)))

(defn part2 []
  (let [outputs (:outputs (run-computer computer [5]))]
    (last outputs)))

(defn main
  []
  (println "P1:" (part1))
  (println "P2:" (part2)))
