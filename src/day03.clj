(ns day03
  (:require [clojure.string :as str]))

(def small-input (str/split-lines "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"))

(def input (->> "resources/day03.txt"
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

(defn find-n-with [input f]
  (reduce (fn [ns i]
            (if (next ns)
              (let [[_ c] (->> (frequencies-at ns i)
                               (map (comp vec reverse))
                               sort
                               f)]
                (filter #(-> % (nth i) (= c)) ns))
              (reduced (first ns))))
          input
          (range (count input))))

(defn part-2 [input]
  (->> [last first]
       (map #(find-n-with input %))
       (map #(Long/parseLong % 2))
       (reduce *)))

(comment

 (part-2 small-input)

 (part-2 input)                                             ;; 6822109
 )
