(ns day03
  (:require
    [common]
    [clojure.set :as set])
  (:gen-class))


(defn- rucksack-compartments
  [rucksack]
  (split-at (/ (count rucksack) 2) rucksack))

(defn- common-item
  [[comp1 comp2]]
  (first (set/intersection (set comp1) (set comp2))))

(def item-priorities
  (zipmap
    [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z]
    (range 1 53)))

(defn part1
  [lines]
  (->>
    lines
    (map (comp item-priorities common-item rucksack-compartments))
    (reduce +)))

(defn- possible-badge
  [[elf1 elf2 elf3]]
  (first (set/intersection (set elf1) (set elf2) (set elf3))))

(defn part2 [lines]
  (->>
    lines
    (partition 3)
    (map (comp item-priorities possible-badge))
    (reduce +)))


(comment
  (def sample (common/sample->lines "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"))

  (assert (= (part1 sample) 157))
  (assert (= (part2 sample) 70)))


(defn -main
  [& args]
  (common/puzzle part1 part2 args))

