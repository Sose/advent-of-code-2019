(ns aoc.day09
  (:require [aoc.core :as core]
            [clojure.string :as s]))

(def input-program (apply vector (map read-string (s/split (core/input-for 9) #","))))

;;(def input-program [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])
;;(def input-program [1102,34915192,34915192,7,4,7,99,0])
;;(def input-program [104,1125899906842624,99])

(defn make-memory
  [input n-empty]
  (into [] (concat input (repeat n-empty 0))))

(def initial-computer
  {:memory (make-memory input-program 1000) ;; 1000 empty elems
   :ip 0 ;; instruction pointer
   :rb 0 ;; relative base
   :inputs []
   :outputs []})

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

(defn param-value
  [{:keys [memory rb]} param-mode value]
  (case param-mode
    0 (nth memory value)        ;; parameter mode
    1 value                     ;; immediate mode
    2 (nth memory (+ value rb)) ;; relative mode
    (str "Error in param-value! mode was: " param-mode)))

(defn param-value-output
  [{:keys [memory rb]} param-mode value]
  (case param-mode
    0 value
    1 (str "ERROR!")
    2 (+ value rb)))

(defn parse-opcode
  "Returns [opcode, modes]"
  [code]
  (let [[a b c d e] (core/left-pad (core/digits code) 5 0)]
    [(+ e (* d 10)) ;;opcode
     [c b a]]))     ;;modes

;; just for fun
(def parse-opcode-memoized (memoize parse-opcode))

(defn set-inputs
  [computer inputs]
  (assoc computer :inputs inputs))

(defn clear-output
  [computer]
  (assoc computer :outputs []))

;; Instructions / operations

(defn binary-instr
  "Starting from ip+1, reads 2 input addresses and 1 output address.
  Updates memory at output address and advances ip by 4."
  [{:keys [memory ip rb] :as computer} modes func]
  (let [[a-addr b-addr out-addr] (read-values computer 3)
        a (param-value computer (nth modes 0) a-addr)
        b (param-value computer (nth modes 1) b-addr)
        out (param-value-output computer (nth modes 2) out-addr)]
    (-> computer
        (update-memory out (func a b))
        (advance-ip 4))))

(defn instr-add
  [computer modes]
  (binary-instr computer modes +))

(defn instr-mul
  [computer modes]
  (binary-instr computer modes *))

(defn instr-input
  "Consumes one input and writes it to out-addr"
  [{:keys [memory ip inputs] :as computer} modes]
  (let [out-addr (first (read-values computer 1))
        input-val (first inputs)
        out (param-value-output computer (nth modes 0) out-addr)]
    (-> computer
        (update-memory out input-val)
        (update :inputs next)
        (advance-ip 2))))

(defn instr-output
  "Appends one output"
  [{:keys [memory ip outputs] :as computer} modes]
  (let [out-addr (first (read-values computer 1))
        out-val (param-value computer (first modes) out-addr)]
    (-> computer
        (update :outputs conj out-val)
        (advance-ip 2))))

;; helper for jump-if-true and jump-if-false
(defn helper-jump-fn
  "Jumps.. TODO: better doc"
  [{:keys [memory ip outputs] :as computer} modes compare-fn]
  (let [[check-addr newip-addr] (read-values computer 2)
        check (param-value computer (nth modes 0) check-addr)
        newip (param-value computer (nth modes 1) newip-addr)]
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
        a (param-value computer (nth modes 0) a-addr)
        b (param-value computer (nth modes 1) b-addr)
        out (param-value-output computer (nth modes 2) out-addr)]
    (if (compare-fn a b)
      (-> computer
          (update-memory out 1)
          (advance-ip 4))
      (-> computer
          (update-memory out 0)
          (advance-ip 4)))))

(defn instr-less-than
  [computer modes]
  (helper-compare-and-set-fn computer modes <))

(defn instr-equals
  [computer modes]
  (helper-compare-and-set-fn computer modes =))

(defn instr-add-relative-base
  [computer modes]
  (let [[a-addr] (read-values computer 1)
        a (param-value computer (nth modes 0) a-addr)]
    (-> computer
        (update-in [:rb] + a)
        (advance-ip 2))))

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
   9 instr-add-relative-base
   99 instr-halt})

;; Running the computer

(defn step-computer
  "Execute one step, returning a new computer state as output"
  [{:keys [memory ip] :as computer}]
  (let [[opcode modes] (parse-opcode (nth memory ip))
        instr (op->instr opcode)]
    (instr computer modes)))

(defn run-computer-until
  "Runs a computer until (test-fn computer) is true"
  [computer test-fn]
  (->> (iterate step-computer computer)
       (drop-while (complement test-fn))
       first))

(defn run-computer
  "Runs a computer until it halts by setting :halt"
  ([computer]
   (run-computer-until computer :halt))
  ([computer inputs]
   (run-computer (update-in computer [:inputs] #(vec (concat %1 %2)) inputs))))

(defn run-computer-until-output
  "Runs a computer until it halts or an output is ready"
  ([computer]
   (run-computer-until computer #(or (:halt %) (-> % :outputs not-empty))))
  ([computer inputs]
   (run-computer-until-output (update-in computer [:inputs] #(vec (concat %1 %2)) inputs))))

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
  (run-computer (set-inputs initial-computer [1])))

;; Part 2

(defn part2
  []
  (run-computer (set-inputs initial-computer [2])))

(defn main
  []
  (println "P1:" (time (part1)))
  (println "P2:" (time (part2))))
