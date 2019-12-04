(ns aoc.day02
  (:require [aoc.core :as core]
            [clojure.string :as s]))

(def input-state (apply vector (map read-string (s/split (core/input-for 02) #","))))

(defn eval-state
  [state index]
  (let [opcode (nth state index)]
    (if (= 99 opcode)
      state
      (let [input-positions (subvec state (+ index 1) (+ index 3))
            input-values (map #(nth state %) input-positions)
            output-position (nth state (+ index 3))
            new-index (+ index 4)]
        (case opcode
          1 (eval-state (assoc state output-position (apply + input-values))
                        new-index)
          2 (eval-state (assoc state output-position (apply * input-values))
                        new-index))))))


(defn run-program
  "Runs a program given input state and optionally a noun and a verb (defaults 12, 2)"
  ([state]
   (run-program state 12 2))
  ([state noun verb]
   (eval-state (assoc state 1 noun 2 verb) 0)))

(defn answer
  ([state]
   (answer state 12 2))
  ([state noun verb]
   (first (run-program state noun verb))))

;; part 2

(def desired-output 19690720)

(defn find-noun-and-verb
  [desired-output]
  (for [noun (range 0 100)
        verb (range 0 100)
        :when (= desired-output (answer input-state noun verb))]
    (+ verb (* 100 noun))))

;; Main

(defn main
  []
  (find-noun-and-verb desired-output))
