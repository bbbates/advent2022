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


(defn- trees-view-distances
  [height-map row [row-idx col-idx] tree-height]
  [
   ; looking left
   (if (zero? col-idx)
     0
     (let [to-the-left (reverse (subvec row 0 col-idx))
           left-score (take-while #(> tree-height %) to-the-left)]
       (if (= (count to-the-left) (count left-score))
         (count to-the-left)
         (inc (count left-score)))))

   ; ooking right
   (if (= (inc col-idx) (count row))
     0
     (let [to-the-right (subvec row (inc col-idx) (count row))
           right-score (take-while #(> tree-height %) to-the-right)]
       (if (= (count to-the-right) (count right-score))
         (count to-the-right)
         (inc (count right-score)))))

   ; looking up
   (if (zero? row-idx)
     0
     (let [up (map #(% col-idx) (reverse (subvec height-map 0 row-idx)))
           up-score (take-while #(> tree-height %) up)]
       (if (= (count up) (count up-score))
         (count up)
         (inc (count up-score)))))

   ; looking down
   (if (= (inc row-idx) (count row))
     0
     (let [down (map #(% col-idx) (subvec height-map (inc row-idx) (count height-map)))
           down-score (take-while #(> tree-height %) down)]
       (if (= (count down) (count down-score))
         (count down)
         (inc (count down-score)))))])


(defn scenic-scores [height-map]
   (map-indexed (fn [idx row] (map-indexed #(trees-view-distances height-map row [idx %1] %2) row))
                         height-map))

(defn part2
  [lines]
  (let [height-map (mapv parse-row lines)]
    (reduce max (map (partial apply *)
                     (apply concat (scenic-scores height-map))))))

(def sample (common/sample->lines "30373\n25512\n65332\n33549\n35390\n"))

(assert (= 21 (part1 sample)))
(assert (= 8 (part2 sample)))

(defn -main
  [& args]
  (common/puzzle part1 part2 args))
