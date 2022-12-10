(ns day09
  (:require
    [common])
  (:gen-class))


(def dir {"R" :right "U" :up "L" :left "D" :down})
(def move-delta {:right [1 0] :up [0 1] :left [-1 0] :down [0 -1]})

(defn- parse-head-movement
  [line]
  (let [[_ dirs num] (re-find #"(R|D|L|U) (\d+)" line)]
    (repeat (parse-long num) (dir dirs))))

(defn- overlapping? [head tail] (= head tail))
(defn- adjacent? [[head-x head-y :as head] [tail-x tail-y :as tail]]
  (and
    (not (overlapping? head tail))
    (< (abs (- head-x tail-x)) 2)
    (< (abs (- head-y tail-y)) 2)))

(defn- diagonally-adjacent? [[head-x head-y :as head] [tail-x tail-y :as tail]]
  (and
    (not (overlapping? head tail))
    (= (abs (- head-x tail-x)) 1)
    (= (abs (- head-y tail-y)) 1)))


(defn- move-head [[head-x head-y :as head] tail dir]
  (let [[dx dy] (move-delta dir)
        new-head [(+ head-x dx) (+ head-y dy)]
        new-tail (cond
                   (overlapping? head tail) tail
                   (overlapping? new-head tail)  tail
                   (adjacent? new-head tail)  tail
                   (diagonally-adjacent? head tail)  head
                   (diagonally-adjacent? new-head tail)  tail
                   (adjacent? head tail) head)]
    [new-head new-tail]))

(move-head [1 3] [2 4] :right)

(defn- apply-movements
  [head-movements]
  (reductions (fn [[head tail] dir]
                (move-head head tail dir))
              [[0 0] [0 0]] head-movements))

(defn part1
  [lines]
  (let [head-movements (mapcat parse-head-movement lines)
        all-positions (apply-movements head-movements)]
    (count (set (map second all-positions)))))

(defn part2
  [lines]

  )


(def sample (common/sample->lines "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"))

(assert (= 13 (part1 sample)))

(defn -main
  [& args]
  (common/puzzle part1 part2 args))
