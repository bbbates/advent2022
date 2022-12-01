(ns day01
  (:import [java.io BufferedReader StringReader])
  (:gen-class))


(defn- split-by-elves
  [lines]
  (->>
    lines
    (partition-by #(empty? (clojure.string/trim %)))
    (filter #(not= (first %) ""))))

(defn- elf-calories [bag]
  (reduce + (map parse-long bag)))

(defn part1 [lines]
  (->>
    lines
    (split-by-elves)
    (map elf-calories)
    (reduce max)))

(defn part2 [lines]
  (->>
    lines
    (split-by-elves)
    (map elf-calories)
    (sort)
    (reverse)
    (take 3)
    (reduce +)))


(comment
  (def sample
    (line-seq
      (BufferedReader.
        (StringReader. "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"))))

  (part1 sample)
  (part2 sample))


(defn -main
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [lines (line-seq rdr)]
      (print
        (if (= (first args) "1")
          (part1 lines)
          (part2 lines))))))
