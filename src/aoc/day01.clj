(ns aoc.day01
  (:require [aoc.core :as core]
            [clojure.string :as s]))

;; Part 1

(defn fuel-required
  "Fuel required = divide by 3, round down, subtract 2"
  [mass]
  (- (quot mass 3) 2))

(defn masses
  [input]
  (map read-string (s/split-lines input)))

(defn fuel-for-file
  [input]
  (let [masses (masses input)
        fuels (map fuel-required masses)]
    (apply + fuels)))

;; Part 2

(defn fuel-required-2
  "Fuel required taking into account the fuel needed for the fuel itself"
  [mass]
  (let [fuel-req (fuel-required mass)]
    (if (< fuel-req 0)
      0
      (+ fuel-req (fuel-required-2 fuel-req)))))

(defn fuel-for-file-2
  [input]
  (let [masses (masses input)
        fuels (map fuel-required-2 masses)]
    (apply + fuels)))

;; Main

(defn main []
  (let [input (core/input-for 01)]
    (println (str "P1: " (fuel-for-file input)))
    (println (str "P2: " (fuel-for-file-2 input)))))
