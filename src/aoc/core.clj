(ns aoc.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn input-for
  [day]
  (-> (format "day%02d.txt" day)
      io/resource
      slurp))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
