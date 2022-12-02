(ns day02
  (:require [common])
  (:gen-class))


(defn parse-round [r]
  (clojure.string/split r #" "))

(def choice-score {"X" 1 "Y" 2 "Z" 3})

(def outcome-score
  {
   ["A" "X"] 3
   ["A" "Y"] 6
   ["A" "Z"] 0

   ["B" "X"] 0
   ["B" "Y"] 3
   ["B" "Z"] 6

   ["C" "X"] 6
   ["C" "Y"] 0
   ["C" "Z"] 3})

(defn round-score [[_ me :as r]]
  (+ (choice-score me) (outcome-score r)))

(defn part1
  [lines]
  (reduce + (map (comp round-score parse-round) lines)))


; X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
(def needs-to-end
  {
   ; rock
   ["A" "X"] ["A" "Z"]
   ["A" "Y"] ["A" "X"]
   ["A" "Z"] ["A" "Y"]

   ; paper
   ["B" "X"] ["B" "X"]
   ["B" "Y"] ["B" "Y"]
   ["B" "Z"] ["B" "Z"]

   ; scissors
   ["C" "X"] ["C" "Y"]
   ["C" "Y"] ["C" "Z"]
   ["C" "Z"] ["C" "X"]})


(defn part2
  [lines]
  (reduce + (map (comp round-score needs-to-end parse-round) lines)))

(comment
  (def sample (common/sample->lines "A Y\nB X\nC Z"))
  (part1 sample)
  (part2 sample))

(defn -main
  [& args]
  (common/puzzle part1 part2 args))
