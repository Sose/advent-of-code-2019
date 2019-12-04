(ns aoc.day04)

(def all-nums (range 156218 652528)) ;; puzzle input is 156218-652527

(defn digits [number] (map #(Character/digit % 10) (str number)))

(defn check-password
  [digits]
  (if-not (= (count digits) 6)
    false
    (loop [prev (first digits)
           [cur & tail] (rest digits)
           found-2-same false]
      (cond
        (nil? cur) found-2-same
        (> prev cur) false
        :else (recur cur tail (or found-2-same (= prev cur)))))))

(defn problem1
  []
  (->> all-nums
       (map digits)
       (filter check-password)
       (count)))

;; --------- Part 2 ------------

(defn check-password-2
  [digits]
  (if-not (= (count digits) 6)
    false
    (loop [prev (first digits)
           [cur & tail] (rest digits)
           found-2-same false
           n-same 1]
      (let [is-same (= prev cur)]
        (cond
          (nil? cur) (or found-2-same (= n-same 2))
          (> prev cur) false
          :else (recur cur
                       tail
                       (if is-same found-2-same (or found-2-same (= n-same 2)))
                       (if is-same (+ n-same 1) 1)))))))


(defn problem2
  []
  (->> all-nums
       (map digits)
       (filter check-password-2)
       (count)))

;; Main

(defn main
  []
  (println "This takes around 40 seconds for both parts..")
  (println (time (str "P1: " (problem1))))
  (println (time (str "P2: " (problem2)))))
