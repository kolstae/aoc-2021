(ns day07
  (:require [clojure.string :as str]))

(def small-input [16 1 2 0 4 2 7 1 2 14])

(def input (mapv parse-long (str/split (str/trim (slurp "resources/day07.txt")) #",")))

(defn min-fuel-use [fuel-f ns]
  (->> (range (apply min ns) (apply max ns))
       (map (fn [p] (reduce + (map #(fuel-f (- p %)) ns))))
       (apply min)))

(comment

 (min-fuel-use (fn [^long d] (Math/abs d)) input)           ;; 352707
 )

(def n! (memoize (fn [n] (reduce + (range (inc n))))))

(comment

 (min-fuel-use (fn [^long d] (n! (Math/abs d))) input)      ;; 95519693
 )
