(ns day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def small-input (str/split-lines "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"))

(def input (->> "day03.txt"
                io/resource
                slurp
                str/split-lines))

(defn part-1 [input]
  (let [xs (->> input
                (apply interleave)
                (partition (count input))
                (map frequencies)
                (map #(sort-by second %)))]
    (->> [(map (comp first second) xs) (map ffirst xs)]
         (map (comp #(Long/parseLong % 2) str/join))
         (reduce *))))

(comment

 (part-1 input)                                             ;; 2640986
 )

(defn frequencies-at [input i]
  (let [cs (->> input
                (apply interleave)
                (partition (count input)))]
    (frequencies (nth cs i))))

(defn find-n-with [input [f tie]]
  (reduce (fn [ns i]
            (if (= 1 (count ns))
              (reduced (first ns))
              (let [fs (frequencies-at ns i)
                    [c] (if (apply = (vals fs))
                          [tie]
                          (apply f val fs))]
                (filter #(= c (nth % i)) ns))))
          input
          (range (count input))))

(defn part-2 [input]
  (->> [[max-key \1] [min-key \0]]
       (map #(find-n-with input %))
       (map #(Long/parseLong % 2))
       (reduce *)))

(comment

 (part-2 input)                                             ;; 6822109
 )
