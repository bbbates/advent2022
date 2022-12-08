(ns day08
  (:require
    [common])
  (:gen-class))

(defn- parse-row
  [line]
  (mapv #(parse-long (str %)) line))


(defn- tree-visible?
  [height-map row [row-idx col-idx] tree-height]
  (or
    ; an edge
    (or (zero? row-idx) (zero? col-idx)
        (= (inc row-idx) (count height-map))
        (= (inc col-idx) (count row)))

    ; trees in row to the left
    (> tree-height (apply max 0 (subvec row 0 col-idx)))

    ; trees in row to the right
    (> tree-height (apply max 0 (subvec row (inc col-idx) (count row))))

    ; trees in col up
    (> tree-height (apply max 0 (map #(% col-idx) (subvec height-map 0 row-idx))))

    ; trees in col down
    (> tree-height (apply max 0 (map #(% col-idx) (subvec height-map (inc row-idx) (count height-map)))))))

(defn- trees-visible-in-row
  [height-map row idx]
  ;(println (map-indexed #(tree-visible? height-map row [idx %1] %2) row))
  (count (filter identity (map-indexed #(tree-visible? height-map row [idx %1] %2) row))))

(defn- trees-visible
  [height-map]
  (first
    (reduce
      (fn [[acc idx] row]
        [(+ acc (trees-visible-in-row height-map row idx)) (inc idx)])
      [0 0] height-map)))

(defn part1
  [lines]
  (let [height-map (mapv parse-row lines)]
    (trees-visible height-map)))

(defn part2
  [lines]
  )


(def sample (common/sample->lines "30373\n25512\n65332\n33549\n35390\n"))

(assert (= 21 (part1 sample)))

(defn -main
  [& args]
  (common/puzzle part1 part2 args))
