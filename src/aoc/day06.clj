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
K)L
K)YOU
I)SAN")

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

;; part 2

(defn find-val
  [orbits tofind]
  (->> orbits
       (filter (fn [[k v]] (get v tofind)))
       first
       first))

(defn route-from-to
  [orbits from to]
  (loop [from from
         result '()]
    (if (= from to)
      result
      (let [found (find-val orbits from)]
        (recur found
               (conj result found))))))

;;(route-from-to orbits "SAN" "COM")

(defn first-common-orbit
  [route1 route2]
  (loop [route1 route1
         depth 0]
    (if-let [found (some #{(first route1)} route2)]
      (list found depth)
      (recur (rest route1)
             (inc depth)))))


(defn part2
  []
  (let [orbits (orbits input)
        santaroute (reverse (route-from-to orbits "SAN" "COM"))
        youroute (reverse (route-from-to orbits "YOU" "COM"))]
    (+ (second (first-common-orbit santaroute youroute))
       (second (first-common-orbit youroute santaroute)))))

;; main

(defn main
  []
  (println "P1:" (part1))
  (println "P2:" (part2)))
