(ns day09
  (:require [clojure.string :as str]))

(def small-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")

(def input (str/trim (slurp "resources/day09.txt")))

(defn parse [input]
  (mapv (partial mapv (comp #(- % (int \0)) int)) (str/split-lines input)))

(defn low-point [m x y h]
  (->> [[y (inc x)] [y (dec x)] [(inc y) x] [(dec y) x]]
       (keep (partial get-in m))
       (every? (partial < h))))

(defn part-1 [input]
  (let [m (parse input)]
    (reduce + (for [x (range (count (first m))) y (range (count m))
                    :let [h (get-in m [y x])]
                    :when (low-point m x y h)]
                (inc h)))))

(comment

 (part-1 input)                                             ;; 594
 )

(defn adjacent [m [x y]]
  (filter (fn [[x y]] (when-let [h (get-in m [y x])] (< h 9)))
          [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

(defn find-basin [m p]
  (loop [b #{} q [p] done #{}]
    (if-let [p (first q)]
      (let [done (conj done p)
            q (concat (next q) (remove done (adjacent m p)))]
        (recur (conj b p) q done))
      b)))

(defn part-2 [input]
  (let [m (parse input)
        lps (for [x (range (count (first m))) y (range (count m))
                  :let [h (get-in m [y x])]
                  :when (low-point m x y h)]
              [x y])]
    (->> lps
         (map (partial find-basin m))
         (map count)
         (sort >)
         (take 3)
         (reduce *))))

(comment

 (part-2 input)                                             ;; 858494
 )
