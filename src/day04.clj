(ns day04
  (:require
    [common]
    [clojure.set :as set])
  (:gen-class))


(defn- parse-range
  [sections]
  (let [[s e] (clojure.string/split sections #"-")]
    (set (range (parse-long s) (inc (parse-long e))))))

(defn- section-pairs
  [line]
  (mapv parse-range (clojure.string/split line #",")))

(defn- fully-overlaps?
  [[sec1 sec2]]
  (let [in-common (clojure.set/intersection sec1 sec2)]
    (or (= (count in-common) (count sec1))
        (= (count in-common) (count sec2)))))

(defn part1
  [lines]
  (let [pairs (map section-pairs lines)]
    (count (filter fully-overlaps? pairs))))

(defn part2
  [lines]
  (let [pairs (map section-pairs lines)]
    (count (filter #(not-empty (apply clojure.set/intersection %)) pairs))))


(comment
  (def sample (common/sample->lines "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"))

  (assert (= (part1 sample) 2))
  (assert (= (part2 sample) 4)))


(defn -main
  [& args]
  (common/puzzle part1 part2 args))

