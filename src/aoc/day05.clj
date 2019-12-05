(ns aoc.day05
  (:require [aoc.core :as core]
            [clojure.string :as s]))

(def input-state (apply vector (map read-string (s/split (core/input-for 02) #","))))
;;(def input-state [1,9,10,3,2,3,11,0,99,30,40,50])

(def computer
  {:memory input-state
   :ip 0 ;; instruction pointer
   })

;; Helper functions

(defn advance-ip
  "Advances instruction pointer by n, returns new computer state"
  [computer n]
  (update-in computer [:ip] + n))

(defn update-memory
  [{:keys [memory] :as computer} addr value]
  (let [new-mem (assoc memory addr value)]
    (assoc computer :memory new-mem)))

(defn read-values
  "Reads n number of values from memory, starting at ip+1"
  [{:keys [memory ip]} n]
  (subvec memory (inc ip) (+ ip n 1)))

;; Instructions / operations

(defn binary-instr
  "Starting from ip+1, reads 2 input addresses and 1 output address.
  Updates memory at output address and advances ip by 4."
  [{:keys [memory ip] :as computer} func]
  (let [[a-addr b-addr out-addr] (read-values computer 3)
        a (nth memory a-addr)
        b (nth memory b-addr)]
    (-> computer
        (update-memory out-addr (func a b))
        (advance-ip 4))))

(defn instr-add
  [computer]
  (binary-instr computer +))

(defn instr-mul
  [computer]
  (binary-instr computer *))

(defn instr-halt
  [computer]
  (assoc computer :halt true))

(def op->instr
  {1 instr-add
   2 instr-mul
   99 instr-halt})

;; Running the computer

(defn step-computer
  "Execute one step, returning a new computer state as output"
  [{:keys [memory ip] :as computer}]
  (let [opcode (nth memory ip)
        instr (op->instr opcode)]
    (instr computer)))

(defn run-computer
  [computer]
  (loop [c computer]
    (if (:halt c)
      c
      (recur (step-computer c)))))

(defn answer
  "Runs a computer and returns the value at addr 0 in memory.
  Optionally takes 2 extra args that replace addresses 1 and 2 before running"
  ([computer]
   (-> (run-computer computer)
       :memory
       first))
  ([computer noun verb]
   (answer (-> computer
               (update-memory 1 noun)
               (update-memory 2 verb)))))

;; part 1

(defn part1 []
  (answer computer 12 2))

;; part 2

(def desired-output 19690720)

(defn find-noun-and-verb
  [computer desired-output]
  (first (for [noun (range 0 100)
               verb (range 0 100)
               :when (= desired-output (answer computer noun verb))]
           (+ verb (* 100 noun)))))

(defn part2 []
  (find-noun-and-verb computer desired-output))

;; Main

(defn main
  []
  (println "P1:" (part1))
  (println "P2:" (part2)))
