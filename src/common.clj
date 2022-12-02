(ns common
  (:import [java.io BufferedReader StringReader]))

(defn puzzle
  [part1-fn part2-fn args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [lines (line-seq rdr)]
      (print
        (if (= (first args) "1")
          (part1-fn lines)
          (part2-fn lines))))))

(defn sample->lines
  [s]
  (line-seq (BufferedReader. (StringReader. s))))
