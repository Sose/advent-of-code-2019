(ns aoc.day09
  (:require [aoc.core :as core]
            [clojure.string :as s]
            [aoc.intcode :as intcode]))

(def input-program (apply vector (map read-string (s/split (core/input-for 9) #","))))

(defn part1
  []
  (-> (intcode/make-computer input-program)
      (intcode/set-inputs [1])
      intcode/run-computer
      :outputs
      last))

;; Part 2

(defn part2
  []
  (-> (intcode/make-computer input-program)
      (intcode/set-inputs [2])
      intcode/run-computer
      :outputs
      last))

(defn main
  []
  (println "P1:" (time (part1)))
  (println "P2:" (time (part2))))
