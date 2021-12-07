(ns day07
  (:require [clojure.string :as str]))

(def small-input [16 1 2 0 4 2 7 1 2 14])

(def input (mapv parse-long (str/split (str/trim (slurp "resources/day07.txt")) #",")))

(defn part-1 [ns]
  (let [avg (long (/ (reduce + ns) (count ns)))]
    (->> (interleave (range avg (apply max ns))
                     (range (dec avg) (apply min ns) -1))
         (map (fn [p] [(reduce + (map #(Math/abs ^long (- p %)) ns)) p]))
         (apply min-key first)
         first)))

(comment

 (part-1 input)                                             ;; 352707
 )

(def n! (memoize (fn [n] (reduce + (range (inc n))))))

(defn part-2 [ns]
  (let [avg (long (/ (reduce + ns) (count ns)))]
    (->> (interleave (range avg (apply max ns))
                     (range (dec avg) (apply min ns) -1))
         (map (fn [p] [(reduce + (map #(n! (Math/abs ^long (- p %))) ns)) p]))
         (apply min-key first)
         first)))

(comment

 (part-2 input)                                             ;; 95519693
 )
