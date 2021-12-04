(ns day04
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def small-input (str/split-lines "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"))

(def input (->> "day04.txt"
                io/resource
                slurp
                str/split-lines))

(defn parse [input]
  (let [[[ns] & bs] (take-nth 2 (partition-by empty? input))
        split-to-long (fn [s] (keep parse-long (str/split s #"[,\s]+")))]
    [(split-to-long ns)
     (map (fn [b] (map #(split-to-long %) b)) bs)]))

(defn b->sets [b]
  (into (mapv set b)
        (apply map (comp set vector) b)))

(defn score [drawn b-sets]
  (->> (mapcat identity b-sets)
       (remove (set drawn))
       distinct
       (reduce +)
       (* (last drawn))))

(defn part-1 [input]
  (let [[ns bs] (parse input)
        bs-sets (map b->sets bs)
        [drawn b] (loop [more (drop 5 ns)
                         seen (vec (take 5 ns))]
                    (let [[winner] (filter (partial some (partial set/superset? (set seen))) bs-sets)]
                      (if winner
                        [seen winner]
                        (recur (next more) (conj seen (first more))))))]
    (score drawn b)))

(comment

 (part-1 input)                                             ;; 22680
 )

(defn part-2 [input]
  (let [[ns bs] (parse input)
        [drawn b] (loop [more (drop 5 ns)
                         seen (vec (take 5 ns))
                         bs-sets (map b->sets bs)]
                    (let [losers (remove (partial some (partial set/superset? (set seen))) bs-sets)]
                      (if (empty? losers)
                        [seen (first bs-sets)]
                        (recur (next more) (conj seen (first more)) losers))))]
    (score drawn b)))

(comment

 (part-2 input)                                             ;; 16168
 )
