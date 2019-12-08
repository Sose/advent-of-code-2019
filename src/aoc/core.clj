(ns aoc.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn input-for
  [day]
  (-> (format "day%02d.txt" day)
      io/resource
      slurp))

;; https://stackoverflow.com/questions/29929325/how-to-split-a-number-in-clojure
(defn digits [n]
  "Split a positive integer into list of digits"
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
