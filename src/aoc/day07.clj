(ns aoc.day07
  (:require [aoc.core :as core]
            [clojure.string :as s]))

(def input-state (apply vector (map read-string (s/split (core/input-for 07) #","))))
;;(def input-state [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
;;(def input-state [3,23,3,24,1002,24,10,24,1002,23,-1,23,
;;                  101,5,23,23,1,24,23,23,4,23,99,0,0])

;; (def input-state [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
;;                   -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
;;                   53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])

;;(def input-state [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
;;                  27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])


(def computer
  {:memory input-state
   :ip 0 ;; instruction pointer
   :inputs []
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

(defn param-value
  [memory param-mode value]
  (case param-mode
    0 (nth memory value)
    1 value
    (str "Error in param-value! mode was: " param-mode)))

(defn right-pad
  [coll len elem]
  (take len (concat coll (repeat elem))))

(defn left-pad
  "The worst way ever to left-pad something"
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

;; Instructions / operations

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
   (run-computer (update-in computer [:inputs] #(vec (concat %1 %2)) inputs))))

(defn run-computer-until-output
  "Helper for day 7 part2... Executes until halt or an output is ready"
  ([computer]
   (loop [c computer]
     (if (or (:halt c) (-> c :outputs not-empty))
       c
       (recur (step-computer c)))))
  ([computer inputs]
   (run-computer-until-output (update-in computer [:inputs] #(vec (concat %1 %2)) inputs))))

(defn run-with-phase-settings
  [phase-settings]
  (loop [phase-settings phase-settings
         output 0]
    (if (empty? phase-settings)
      output
      (recur (rest phase-settings)
             (-> (run-computer-until-output computer [(first phase-settings), output])
                 :outputs
                 last)))))

(defn all-phase-settings
  [from to]
  (for [a (range from to)
        b (range from to)
        c (range from to)
        d (range from to)
        e (range from to)
        :when (= 5 (count (into #{} [a b c d e])))]
    [a,b,c,d,e]))

(defn part1
  []
  (->> (all-phase-settings 0 5)
       (map run-with-phase-settings)
       (apply max)))

;; Part 2

(defn make-amp
  [computer phase-setting]
  (assoc computer :inputs [phase-setting]))

(defn run-amps-once
  "Takes a list of computers and list of inputs to the first computer as arguments.
  Runs computers until output, passes the output as input to the next computer.
  Repeats until all computers have ran, returns new computer states"
  [computers a-input]
  (loop [new-amps [(run-computer-until-output (first computers) a-input)]
         computers (rest computers)]
    (if (empty? computers)
      new-amps
      (let [input (-> new-amps last :outputs)
            new-c (run-computer-until-output (first computers) input)]
        (recur (conj new-amps new-c)
               (rest computers))))))

(defn clear-output
  [computer]
  (assoc computer :outputs []))

(defn run-amps-feedback
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

(defn part2
  []
  (->> (all-phase-settings 5 10)
       (map run-amps-feedback)
       (apply max)))

(defn main
  []
  (println "P1:" (part1))
  (println "P2:" (part2)))
