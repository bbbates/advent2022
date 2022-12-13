(ns day10
  (:require
    [common]
    [clojure.math :as math])
  (:gen-class))

(defn- parse-instruction
  [line]
  (if-let [[_ addend] (re-find #"addx (-?\d+)" line)]
    [[:addx-start] [:addx-end (parse-long addend)]]
    [[:noop]]))

(def screen-width 40)
(def screen-height 6)

(defn- update-x
  [registers [instr addend]]
  (case instr
    :addx-end (update registers :x + addend)
    registers))

(defn draw-pixel [{:keys [x] :as registers} cycle-idx]
  (let [curr-line-index (math/floor-div cycle-idx screen-width)
        curr-pixel (mod cycle-idx screen-width)
        sprite (set [(dec x) x (inc x)])
        pixel-char (if (sprite curr-pixel) "#" ".")]
    (update-in registers [:crt curr-line-index] conj pixel-char)))

(defn- execute-cycle
  [registers [cycle-idx cycle-instr]]
  (->
    registers
    (draw-pixel cycle-idx)
    (update-x cycle-instr)
    (assoc :last cycle-instr)))

(defn- system-state
  [cycle-instructions]
  (vec (reductions execute-cycle {:x 1 :crt (vec (repeatedly screen-height vector))}
                   (map vector (range) cycle-instructions))))

(defn- calc-strength
  [states acc cycle]
  (let [cycle-idx (dec cycle)
        state (states cycle-idx)]
    (+ acc (* cycle (:x state)))))

(defn part1
  [lines]
  (let [cycles (mapcat parse-instruction lines)
        states (system-state cycles)]
    (reduce (partial calc-strength states)
            0 [20 60 100 140 180 220])))

(defn draw-screen
  [{:keys [crt]}]
  (clojure.string/join "\n" (map clojure.string/join crt)))

(defn part2
  [lines]
  (let [cycles (mapcat parse-instruction lines)
        states (system-state cycles)]
    (draw-screen (last states))))

(def sample (common/sample->lines "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop\n"))
(assert (= (part1 sample) 13140))
(assert (= (part2 sample) "##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######....."))

(defn -main
  [& args]
  (common/puzzle part1 part2 args))

