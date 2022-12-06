(ns day06
  (:require
    [common])
  (:gen-class))


(defn- index-of-sentinel
  [signal num]
  (let [possible-starts (partition-all num 1 signal)]
    (first (keep-indexed
             (fn [i chs]
               (when (= (count (set chs)) num)
                 (+ num i)))
             possible-starts))))

(defn part1
  [[signal]]
  (index-of-sentinel signal 4))

(defn part2
  [[signal]]
  (index-of-sentinel signal 14))


(def sample1 (common/sample->lines "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
(def sample2 (common/sample->lines "bvwbjplbgvbhsrlpgdmjqwftvncz"))
(def sample3 (common/sample->lines "nppdvjthqldpwncqszvftbrmjlhg"))
(def sample4 (common/sample->lines "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
(def sample5 (common/sample->lines "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

(assert (= 7 (part1 sample1)))
(assert (= 5 (part1 sample2)))
(assert (= 6 (part1 sample3)))
(assert (= 10 (part1 sample4)))
(assert (= 11 (part1 sample5)))

(assert (= 19 (part2 sample1)))
(assert (= 23 (part2 sample2)))
(assert (= 23 (part2 sample3)))
(assert (= 29 (part2 sample4)))
(assert (= 26 (part2 sample5)))

(defn -main
  [& args]
  (common/puzzle part1 part2 args))
