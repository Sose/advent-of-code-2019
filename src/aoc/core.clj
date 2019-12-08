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

(defn right-pad
  "Right-pads a coll until len items, with elem.
  Returns len items."
  [coll len elem]
  (take len (concat coll (repeat elem))))

(defn left-pad
  "Left-pads a coll until len items, with elem.
  If len <= (count coll), just returns coll."
  [coll len elem]
  (concat (repeat (- len (count coll)) elem)
          coll))
