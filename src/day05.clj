(ns day05
  (:require [clojure.string :as str]))

(def small-input (str/split-lines "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"))

(def input (str/split-lines (slurp "resources/day05.txt")))

(defn parse [input]
  (->> input
       (map #(re-seq #"(\d+),(\d+)\s*->\s*(\d+),(\d+)" %))
       (map (comp (partial partition 2) (partial map parse-long) next first))))

(defn range-for [a b]
  (if (< a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn line->ps [[[x1 y1] [x2 y2]]]
  (let [xs (range-for x1 x2)
        ys (range-for y1 y2)]
    (if (< (count xs) (count ys))
      (map vector (cycle xs) ys)
      (map vector xs (cycle ys)))))

(defn part-1 [input]
  (->> (parse input)
       (filter (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))))
       (mapcat line->ps)
       frequencies
       (remove (comp #{1} val))
       count))

(comment

 (part-1 input)                                             ;; 7085
 )

(defn part-2 [input]
  (->> (parse input)
       (mapcat line->ps)
       frequencies
       (remove (comp #{1} val))
       count))

(comment

 (part-2 input)                                             ;; 20271
 )
