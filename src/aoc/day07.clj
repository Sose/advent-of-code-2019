(ns aoc.day07
  (:require [aoc.core :as core]
            [clojure.string :as s]
            [aoc.intcode :refer :all]))

(def input-state (apply vector (map read-string (s/split (core/input-for 07) #","))))

;; (def input-state [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
;;                   -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
;;                   53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])

;;(def input-state [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
;;                  27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])

(def computer (make-computer input-state))

(defn all-phase-settings
  "All possible phase settings. Each integer can only appear once in settings"
  [from to]
  (for [a (range from to)
        b (range from to)
        c (range from to)
        d (range from to)
        e (range from to)
        :when (= 5 (count (into #{} [a b c d e])))]
    [a,b,c,d,e]))

(defn make-amp
  [computer phase-setting]
  (assoc computer :inputs [phase-setting]))

(defn run-amps-once
  "Takes a list of computers and list of inputs to the first computer as arguments.
  Runs first computer until it outputs something, and passes it as input to the next computer.
  Repeats until all computers have ran, returns new computer states."
  [computers a-input]
  (loop [new-amps [(run-computer-until-output (first computers) a-input)]
         computers (rest computers)]
    (if (empty? computers)
      new-amps
      (let [input (-> new-amps last :outputs)
            new-c (run-computer-until-output (first computers) input)]
        (recur (conj new-amps new-c)
               (rest computers))))))

(defn run-amps-feedback
  "Takes a list of amplifier settings as input.
  Runs computers as amplifiers until they halt, returns the last output from last amplifier."
  [amp-settings]
  (loop [computers (map make-amp (repeat computer) amp-settings)
         a-input [0]
         outputs []]
    (let [new-computers (run-amps-once computers a-input)
          new-a-input (-> new-computers last :outputs)]
      (if (some :halt new-computers)
        (last outputs)
        (recur (map clear-output new-computers)
               new-a-input
               (concat outputs new-a-input))))))

;; Part 1

(defn part1
  []
  (->> (all-phase-settings 0 5)
       (map #(map make-amp (repeat computer) %))
       (map #(run-amps-once % [0]))
       (map #(last (:outputs (last %))))
       (apply max)))

;; Part 2

(defn part2
  []
  (->> (all-phase-settings 5 10)
       (map run-amps-feedback)
       (apply max)))

(defn main
  []
  (println "P1:" (time (part1)))
  (println "P2:" (time (part2))))
