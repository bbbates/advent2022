(ns day05
  (:require
    [common])
  (:gen-class))


(defn- crate
  [c]
  (second (re-find #"\[(\w)\]" (apply str c))))

(defn parse-stacks [stacks]
  (let [stack-levels (map (fn [l]
                            (->>
                              l
                              (partition-all 4)
                              (mapv crate)))
                          (butlast stacks))
        total-stacks (count (first stack-levels))]
    (vec (reduce (fn [stacks level]
                   (map-indexed (fn [i s]
                                  (if (level i) (conj s (level i)) s))
                                stacks))
                 (repeatedly total-stacks vector)
                 stack-levels))))

(defn parse-moves [moves]
  (map (fn [move]
         (let [[_ num from to] (re-find #"move (\d+) from (\d+) to (\d+)" move)]
           {:num (parse-long num) :from (parse-long from) :to (parse-long to)}))
       moves))

(defn- parse-input [lines]
  (let [[stacks _ moves] (partition-by #(empty? %) lines)]
    [(parse-stacks stacks) (parse-moves moves)]))


(defn- move
  [stacks {:keys [num from to]}]
  (let [from-stack (stacks (dec from))
        to-stack (stacks (dec to))
        [moving-crates rem-from-stack] (split-at num from-stack)
        rem-to-stack (vec (concat (reverse moving-crates) to-stack))]
    (assoc stacks (dec from) rem-from-stack (dec to) rem-to-stack)))

(defn do-moves [stacks moves]
  (reduce move stacks moves))

;(apply do-moves (parse-input sample))

(defn part1
  [lines]
  (let [[stacks moves] (parse-input lines)
        final-stacks (do-moves stacks moves)]
    (apply str (map first final-stacks))))

(defn part2
  [lines]

  )

(def sample
  (common/sample->lines "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"))

(assert (= "CMZ" (part1 sample)))

(defn -main
  [& args]
  (common/puzzle part1 part2 args))
