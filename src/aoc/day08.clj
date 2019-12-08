(ns aoc.day08
  (:require [aoc.core :as core]
            [clojure.string]))

(defn digits [number] (map #(Character/digit % 10) (str number)))

(def dimensions [25 6]) ;; width x height
(def input-str (clojure.string/trim (core/input-for 8)))
(def input-digits (digits input-str))

(defn layers-from-data
  [data dimensions]
  (partition (apply * dimensions) data))

(defn count-nums
  [layer num]
  (reduce (fn [acc cur] (if (= cur num) (inc acc) acc)) 0 layer))

(defn part1
  []
  (let [layers-w-index (map-indexed vector (layers-from-data input-digits dimensions))
        zeroes (map-indexed (fn [idx item] [idx, (count-nums (second item) 0)]) layers-w-index)
        [least-zeroes-idx _] (apply min-key second zeroes)
        least-zeroes-layer (second (nth layers-w-index least-zeroes-idx))
        ones (count-nums least-zeroes-layer 1)
        twos (count-nums least-zeroes-layer 2)]
    (* ones twos)))

;; Part 2

(defn final-pixel
  "Gets the final pixel value for a list of pixels"
  [pixels]
  (loop [pixels pixels]
    (case (first pixels)
      0 0
      1 1
      2 (recur (rest pixels)))))

(defn final-pixels
  [input-digits dimensions]
  (let [layers (layers-from-data input-digits dimensions)]
    (for [loc (range 0 (apply * dimensions))]
      (let [pixels-at-loc (map #(nth % loc) layers)]
        (final-pixel pixels-at-loc)))))

(defn print-row!
  [row]
  (doseq [pixel row]
    (case pixel
      0 (print " ")
      1 (print "O")))
  (println))

(defn part2
  []
  (let [img (final-pixels input-digits dimensions)
        rows (partition (first dimensions) img)]
    (dorun (map print-row! rows))))

;; Main

(defn main
  []
  (println "P1:" (part1))
  (println "P2:")
  (part2))
