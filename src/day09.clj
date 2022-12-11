(ns day09
  (:require
    [common])
  (:gen-class))


(def dir {"R" :right "U" :up "L" :left "D" :down})
(def move-delta {:right [1 0] :up [0 1] :left [-1 0] :down [0 -1]})

(defn- calc-delta [[this-x this-y] [that-x that-y]]
  [(- this-x that-x) (- this-y that-y)])

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

(defn- move-knot
  [[x y] dir]
  (let [[dx dy] (move-delta dir)]
    [(+ x dx) (+ y dy)]))

(defn- diff-col? [[new-head-x] [tail-x]]
  (not= new-head-x tail-x))

(defn- diff-row? [[_ new-head-y] [_ tail-y]]
  (not= new-head-y tail-y))

(defn- move-tailed-knot
  [orig-head new-head [tail-x tail-y :as tail]]
  (cond
    (overlapping? orig-head tail) tail
    (overlapping? new-head tail) tail
    (adjacent? new-head tail) tail
    (and (diff-col? new-head tail) (not (diff-row? new-head tail))) [(+ tail-x (if (> tail-x (first new-head)) -1 1)) tail-y]
    (and (diff-row? new-head tail) (not (diff-col? new-head tail))) [tail-x (+ tail-y (if (> tail-y (second new-head)) -1 1))]
    (diagonally-adjacent? orig-head new-head) (let [[dx dy] (calc-delta new-head orig-head)] [(+ tail-x dx) (+ tail-y dy)])     ; head moved diagonally
    (diagonally-adjacent? orig-head tail) orig-head
    (diagonally-adjacent? new-head tail) tail))


(defn- move-head-and-tail [head tail dir]
  (let [new-head (move-knot head dir)
        new-tail (move-tailed-knot head new-head tail)]
    [new-head new-tail]))

(defn- apply-movements
  [head-movements]
  (reductions (fn [[head tail] dir]
                (move-head-and-tail head tail dir))
              [[0 0] [0 0]] head-movements))

(defn part1
  [lines]
  (let [head-movements (mapcat parse-head-movement lines)
        all-positions (apply-movements head-movements)]
    (count (set (map second all-positions)))))

(defn- multi-knot-movement
  [knots dir]
  (vec (reduce (fn [acc tail]
                 (if (number? (first acc))
                   (let [new-head (move-knot acc dir)]
                     [new-head (move-tailed-knot acc new-head tail)])

                   (conj acc (move-tailed-knot (knots (dec (count acc))) (last acc) tail))))
               knots)))
(defn- apply-multi-movements
  [head-movements num-knots]
  (reductions multi-knot-movement (vec (repeat num-knots [0 0])) head-movements))

(defn part2
  [lines]
  (let [head-movements (mapcat parse-head-movement lines)
        all-positions (apply-multi-movements head-movements 10)]
    (count (set (map last all-positions)))))

(def sample (common/sample->lines "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"))
(def sample2 (common/sample->lines "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"))

(assert (= 13 (part1 sample)))
(assert (= 1 (part2 sample)))
(assert (= 36 (part2 sample2)))

(defn -main
  [& args]
  (common/puzzle part1 part2 args))
