(ns aoc.main
  (:require [aoc.day01]
            [aoc.day02]
            [aoc.day03]
            [aoc.day04]
            [aoc.day05]
            [aoc.day06]
            [aoc.day07]
            [aoc.day08])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "-- Day 1 --")
  (aoc.day01/main)
  (println "-- Day 2 --")
  (aoc.day02/main)
  (println "-- Day 3 --")
  (aoc.day03/main)
  (println "-- Day 4 --")
  (aoc.day04/main)
  (println "-- Day 5 --")
  (aoc.day05/main)
  (println "-- Day 6 --")
  (aoc.day06/main)
  (println "-- Day 7 --")
  (aoc.day07/main)
  (println "-- Day 8 --")
  (aoc.day08/main))
