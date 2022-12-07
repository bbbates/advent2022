(ns day07
  (:require
    [common])
  (:gen-class))


(defn parse-cmd [line]
  (let [[_ cmd _ args] (re-find #"^\$ (ls|cd)( (.*))?$" line)]
    [(keyword cmd) args]))

(defn parse-dir [line]
  (let [[_ dir-name] (re-find #"^dir (.+)$" line)]
    [:dir dir-name]))

(defn parse-file [line]
  (let [[_ size file-name] (re-find #"^(\d+) (.+)$" line)]
    [:file file-name (parse-long size)]))

(defn- parse-line
  [line]
  (cond
    (clojure.string/starts-with? line "$") (parse-cmd line)
    (clojure.string/starts-with? line "dir") (parse-dir line)
    :else (parse-file line)))


(defn- apply-file-size [tree path file-size]
  (loop [acc-tree tree
         curr-path path]
    (if (empty? curr-path) acc-tree
                           (recur (update-in acc-tree (concat curr-path [::size]) (fnil + 0) file-size)
                                  (pop curr-path)))))

(defn- dir-tree
  [parsed-lines]
  (:tree (reduce (fn [{:keys [curr-path] :as acc} [line-type & args]]
                   (case line-type
                     :cd (if (= ".." (first args))
                           (update acc :curr-path pop)
                           (update acc :curr-path conj (first args)))
                     :file
                     (update acc :tree apply-file-size curr-path (second args))

                     acc)) {:curr-path [] :tree {}} parsed-lines)))

;(dir-tree (map parse-line sample))

(defn sum-dir-sizes [max-size tree]
  (let [acc (atom 0)]
    (clojure.walk/postwalk (fn [node]
                             (when (and (map? node) (< (::size node Integer/MAX_VALUE) max-size))
                               (swap! acc + (::size node)))
                             node)
                           tree)
    @acc))

(defn part1
  [lines]
  (->>
    (map parse-line lines)
    (dir-tree)
    (sum-dir-sizes 100000)))

(defn part2
  [lines]
  )

(def sample (common/sample->lines "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"))

(assert (= 95437 (part1 sample)))

(defn -main
  [& args]
  (common/puzzle part1 part2 args))

