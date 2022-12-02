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
   ["C" "Z"] 3
   })

(defn round-score [[_ me :as r]]
  (+ (choice-score me) (outcome-score r)))

(defn part1
  [lines]
  (reduce + (map (comp round-score parse-round) lines)))

(defn part2
  [lines]

  )

(comment
 (def sample (common/sample->lines "A Y\nB X\nC Z"))

(part1 sample))

(defn -main
  [& args]
  (common/puzzle part1 part2 args))
