(ns aoc.day03
  (:require [aoc.core :as core]
            [clojure.string :as s]
            [clojure.set]))

(def dirs {:L '(-1,  0)
           :R '( 1,  0)
           :U '( 0,  1)
           :D '( 0, -1)})

(defn add-coords
  [[x1 y1] [x2 y2]]
  (list (+ x1 x2) (+ y1 y2)))

(defn mul-coords
  [[x y] v]
  (list (* x v) (* y v)))

(defn wire-line
  [start direction distance]
  (for [i (range distance -1 -1)]
    (add-coords start (mul-coords (direction dirs) i))))

(defn wire-coordinates
  "List of all coordinates for a wire"
  [paths]
  (loop [coords '((0 0))
         paths-to-go paths]
    (if (empty? paths-to-go)
      coords
      (let [[cur-path, & rest-paths] paths-to-go
            [cur-dir, cur-distance] cur-path
            cur-coords (first coords)]
        (recur (concat (butlast (wire-line cur-coords cur-dir cur-distance)) coords)
               rest-paths)))))

(defn wire-from-str
  [w]
  (map
   (fn [x] (list (keyword (str (first x))) (read-string (apply str (rest x)))))
   (s/split w #",")))

(defn coordinate-intersections
  "Intersections for two lists of coordinates, with [0,0] removed"
  [c1 c2]
  (disj
   (clojure.set/intersection (set c1) (set c2))
   [0 0]))

(defn manhattan-distance
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn closest-to-origin
  [intersections]
  (apply min (map manhattan-distance intersections)))

;; -- Problem 2 --

(defn path-distance
  [coords point]
  (->> coords
       (drop-while #(not= point %))
       (count)
       (#(- % 1)))) ;; subtract 1 because of (0,0)

(defn wire-distances
  [wire-coords intersections]
  (map (partial path-distance wire-coords) intersections))

(defn min-steps
  "Closest intersection by walking along the wires"
  [w1 w2 intersections]
  (let [distances1 (wire-distances w1 intersections)
        distances2 (wire-distances w2 intersections)]
    (apply min (map + distances1 distances2))))

;; Main

(defn main
  []
  (let [wire-paths (s/split-lines (core/input-for 03))
        w1 (wire-coordinates (wire-from-str (first wire-paths)))
        w2 (wire-coordinates (wire-from-str (second wire-paths)))
        inters (coordinate-intersections w1 w2)]
    (println (str "P1: " (time (closest-to-origin inters))))
    (println (str "P2: " (time (min-steps w1 w2 inters))))))
