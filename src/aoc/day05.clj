(ns aoc.day05
  (:require [aoc.core :as core]
            [clojure.string :as s]))

(def input-state (apply vector (map read-string (s/split (core/input-for 05) #","))))
;;(def input-state [1,9,10,3,2,3,11,0,99,30,40,50])
;;(def input-state [1002,4,3,4,33])
;;(def input-state [3,0,4,0,99])
;;(def input-state [3,9,8,9,10,9,4,9,99,-1,8])

(def computer
  {:memory input-state
   :ip 0 ;; instruction pointer
   :inputs [1]
   :outputs []})

;; Helper functions

(defn digits [number] (map #(Character/digit % 10) (str number)))

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

(defn param-value
  [memory param-mode value]
  (case param-mode
    0 (nth memory value)
    1 value
    (str "Error in param-value! mode was: " param-mode)))

(defn binary-instr
  "Starting from ip+1, reads 2 input addresses and 1 output address.
  Updates memory at output address and advances ip by 4."
  [{:keys [memory ip] :as computer} modes func]
  (let [[a-addr b-addr out-addr] (read-values computer 3)
        a (param-value memory (nth modes 0) a-addr)
        b (param-value memory (nth modes 1) b-addr)]
    (-> computer
        (update-memory out-addr (func a b))
        (advance-ip 4))))

(defn instr-add
  [computer modes]
  (binary-instr computer modes +))

(defn instr-mul
  [computer modes]
  (binary-instr computer modes *))

(defn instr-input
  [{:keys [memory ip inputs] :as computer} modes]
  (let [out-addr (first (read-values computer 1))
        input-val (first inputs)]
    (-> computer 
        (update-memory out-addr input-val)
        (update :inputs next)
        (advance-ip 2))))

(defn instr-output
  [{:keys [memory ip outputs] :as computer} [mode]]
  (let [out-val (param-value memory mode (first (read-values computer 1)))]
    (-> computer
        (update :outputs conj out-val)
        (advance-ip 2))))

;; helper for jump-if-true and jump-if-false
(defn helper-jump-fn
  "Jumps.. TODO: better doc"
  [{:keys [memory ip outputs] :as computer} modes compare-fn]
  (let [[check-addr newip-addr] (read-values computer 2)
        check (param-value memory (nth modes 0) check-addr)
        newip (param-value memory (nth modes 1) newip-addr)]
    (if (compare-fn check)
      (-> computer (assoc :ip newip))
      (-> computer (advance-ip 3)))))

(defn instr-jump-if-true
  [computer modes]
  (helper-jump-fn computer modes #(not= 0 %)))

(defn instr-jump-if-false
  [computer modes]
  (helper-jump-fn computer modes #(= 0 %)))

(defn helper-compare-and-set-fn
  [{:keys [memory ip outputs] :as computer} modes compare-fn]
  (let [[a-addr b-addr out-addr] (read-values computer 3)
        a (param-value memory (nth modes 0) a-addr)
        b (param-value memory (nth modes 1) b-addr)]
    (if (compare-fn a b)
      (-> computer
          (update-memory out-addr 1)
          (advance-ip 4))
      (-> computer
          (update-memory out-addr 0)
          (advance-ip 4)))))

(defn instr-less-than
  [computer modes]
  (helper-compare-and-set-fn computer modes <))

(defn instr-equals
  [computer modes]
  (helper-compare-and-set-fn computer modes =))

(defn instr-halt
  [computer modes]
  (assoc computer :halt true))

(def op->instr
  {1 instr-add
   2 instr-mul
   3 instr-input
   4 instr-output
   5 instr-jump-if-true
   6 instr-jump-if-false
   7 instr-less-than
   8 instr-equals
   99 instr-halt})

;; Running the computer

(defn right-pad
  [coll len elem]
  (take len (concat coll (repeat elem))))

(defn left-pad
  [coll len elem]
  (-> coll
      reverse
      (right-pad len elem)
      reverse))

(defn parse-opcode
  "Returns [opcode, modes]"
  [code]
  (let [[a b c d e] (left-pad (digits code) 5 0)]
    [(+ e (* d 10)) ;;opcode
     [c b a]]))     ;;modes

(defn step-computer
  "Execute one step, returning a new computer state as output"
  [{:keys [memory ip] :as computer}]
  (let [[opcode modes] (parse-opcode (memory ip))
        instr (op->instr opcode)]
    ;;(println computer)
    (instr computer modes)))

(defn run-computer
  ([computer]
  (loop [c computer]
    (if (:halt c)
      c
      (recur (step-computer c)))))
  ([computer inputs]
   (run-computer (assoc computer :inputs inputs))))

;; Main

(defn part1 []
  (let [outputs (:outputs (run-computer computer [1]))]
    (last outputs)))

(defn part2 []
  (let [outputs (:outputs (run-computer computer [5]))]
    (last outputs)))

;;(defn main
;;  []
;;  (println "P1:" (part1))
;;  (println "P2:" (part2)))
