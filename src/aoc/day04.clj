(ns aoc.day04)

(def all-nums (range 156218 652528)) ;; puzzle input is 156218-652527

(defn digits [number] (map #(Character/digit % 10) (str number)))

(defn check-never-decrease
  [digits]
  (= digits (sort digits)))

(defn check-two-adjacent-same
  [digits]
  (->> digits
       (partition-by identity)
       (map count)
       (some #(>= % 2))))

(defn problem1
  []
  (->> all-nums
       (map digits)
       (filter #(and (check-never-decrease %) (check-two-adjacent-same %)))
       (count)))

;; --------- Part 2 ------------

(defn check-two-adjacent-same-2
  "Checks if two adjacents digits are the same and not part of a larger group"
  [digits]
  (->> digits
       (partition-by identity)
       (map count)
       (some #(= % 2))))

(defn problem2
  []
  (->> all-nums
       (map digits)
       (filter #(and (check-never-decrease %) (check-two-adjacent-same-2 %)))
       (count)))

;; Main

(defn main
  []
  (println "This takes around 40 seconds for both parts..")
  (println (time (str "P1: " (problem1))))
  (println (time (str "P2: " (problem2)))))
