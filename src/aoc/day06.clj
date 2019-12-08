(ns aoc.day06
  (:require [aoc.core :as core]
            [clojure.string]))

(def input (core/input-for 06))

(def example "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

(defn orbits
  "Orbits as hashmap"
  [inputstr]
  (loop [inputs (clojure.string/split-lines inputstr)
         orbits {}]
    (if (empty? inputs)
      orbits
      (let [[left right] (clojure.string/split (first inputs) #"\)")]
        (recur (next inputs)
               (update orbits left (fnil conj #{}) right))))))

(defn count-all-edges [edges root depth]
  (+ depth (reduce (fn [c node]
                     (+ c (count-all-edges edges node (inc depth))))
                   0
                   (edges root))))

(defn part1 []
  (-> input
      (orbits)
      (count-all-edges "COM" 0)))
