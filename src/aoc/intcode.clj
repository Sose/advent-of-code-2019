(ns aoc.intcode
  (:require [aoc.core :as core]))

(defn make-memory
  [input n-empty]
  (into [] (concat input (repeat n-empty 0))))

(defn make-computer
  [input-program]
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

(defn read-memory-with-mode
  "For modes parameter and relative modes, returns the value in the correct memory address.
  For immediate mode, returns just the value"
  [{:keys [memory rb]} param-mode value]
  (case param-mode
    0 (nth memory value)        ;; parameter mode
    1 value                     ;; immediate mode
    2 (nth memory (+ value rb)) ;; relative mode
    (str "Error in read-memory-with-mode! mode was: " param-mode)))

(defn output-addr-with-mode
  "This is meant to be used when getting memory addresses where to write
  because they just need the memory ADDRESS, not the value at the memory address"
  [{:keys [memory rb]} param-mode value]
  (case param-mode
    0 value
    ;; immediate mode (mode 1) is not allowed! (could be self-replace?)
    2 (+ value rb)
    (str "Error in output-addr-with-mode: " param-mode)))

(defn parse-opcode
  "Returns [opcode, modes]"
  [code]
  (let [[a b c d e] (core/left-pad (core/digits code) 5 0)]
    [(+ e (* d 10)) ;;opcode
     [c b a]]))     ;;modes

(def parse-opcode-memoized (memoize parse-opcode)) ;; just for fun (and speed)

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
        a (read-memory-with-mode computer (nth modes 0) a-addr)
        b (read-memory-with-mode computer (nth modes 1) b-addr)
        out (output-addr-with-mode computer (nth modes 2) out-addr)]
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
        out (output-addr-with-mode computer (nth modes 0) out-addr)]
    (-> computer
        (update-memory out input-val)
        (update :inputs next)
        (advance-ip 2))))

(defn instr-output
  "Appends one output"
  [{:keys [memory ip outputs] :as computer} modes]
  (let [out-addr (first (read-values computer 1))
        out-val (read-memory-with-mode computer (first modes) out-addr)]
    (-> computer
        (update :outputs conj out-val)
        (advance-ip 2))))

;; helper for jump-if-true and jump-if-false
(defn helper-jump-fn
  "Reads a value to compare against from ip+1, and new ip from ip+2.
  Jumps by setting ip to new ip if check against compare-value is true."
  [{:keys [memory ip outputs] :as computer} modes compare-fn]
  (let [[check-addr newip-addr] (read-values computer 2)
        check (read-memory-with-mode computer (nth modes 0) check-addr)
        newip (read-memory-with-mode computer (nth modes 1) newip-addr)]
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
  "Reads two values to compare from ip+1 and ip+2.
  Sets addr indicated by ip+3 to 0 or 1"
  [{:keys [memory ip outputs] :as computer} modes compare-fn]
  (let [[a-addr b-addr out-addr] (read-values computer 3)
        a (read-memory-with-mode computer (nth modes 0) a-addr)
        b (read-memory-with-mode computer (nth modes 1) b-addr)
        out (output-addr-with-mode computer (nth modes 2) out-addr)
        value (if (compare-fn a b) 1 0)]
    (-> computer
        (update-memory out value)
        (advance-ip 4))))

(defn instr-less-than
  [computer modes]
  (helper-compare-and-set-fn computer modes <))

(defn instr-equals
  [computer modes]
  (helper-compare-and-set-fn computer modes =))

(defn instr-add-relative-base
  [computer modes]
  (let [[a-addr] (read-values computer 1)
        a (read-memory-with-mode computer (nth modes 0) a-addr)]
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
  (let [[opcode modes] (parse-opcode-memoized (nth memory ip))
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
