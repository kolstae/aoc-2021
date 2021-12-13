(ns day13
  (:require [clojure.string :as str]))

(def small-input "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5")

(def input (str/trim (slurp "resources/day13.txt")))

(def a->i {"x" 0 "y" 1})

(defn parse [input]
  (let [[ps _ is] (partition-by empty? (str/split-lines input))]
    [(mapv (comp (partial mapv parse-long) #(str/split % #",")) ps)
     (map (fn [s] (let [[a n] (drop 2 (str/split s #"[=\s]"))] [a (parse-long n)])) is)]))

(defn fold-at [i n p] (update p i (fn [x] (if (> x n) (- n (- x n)) x))))

(defn fold-ps [ps [a n]] (distinct (map (partial fold-at (a->i a) n) ps)))

(defn part-1 [input]
  (let [[ps is] (parse input)]
    (count (fold-ps ps (first is)))))

(comment

 (part-1 input)                                             ;; 664
 )

(defn part-2 [input]
  (let [[ps is] (parse input)
        ps (reduce fold-ps ps is)
        ls (vec (repeat (inc (apply max (map second ps)))
                        (vec (repeat (inc (apply max (map first ps))) \.))))]
    (doseq [l (reduce (fn [ls [x y]] (assoc-in ls [y x] \#)) ls ps)]
      (println (str/join l)))))

(comment

 (part-2 input)                                             ;; EFJKZLBL
 )
