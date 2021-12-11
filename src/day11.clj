(ns day11
  (:require [clojure.string :as str]))

(def small-input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")

(def input (str/trim (slurp "resources/day11.txt")))

(defn parse [input]
  (mapv (partial mapv (comp #(- % (int \0)) int)) (str/split-lines input)))

(defn adjacent [ns y x]
  (for [x (range (dec x) (+ x 2)) y (range (dec y) (+ y 2))
        :when (and (nat-int? x) (nat-int? y)
                   (< x (count (first ns))) (< y (count ns)))]
    [y x]))

(defn ns->fps [ns]
  (->> (for [x (range (count (first ns))) y (range (count ns))
             :when (= (get-in ns [y x]) 10)]
         (adjacent ns y x))
       (mapcat identity)
       (frequencies)))

(defn step [ns]
  (let [ns (mapv (partial mapv inc) ns)]
    (loop [ns ns fps (ns->fps ns)]
      (if (seq fps)
        (let [ns (reduce (fn [ns [p e]] (update-in ns p #(let [x (+ % e)] (if (< % 10 x) 10 x)))) ns fps)]
          (recur ns (ns->fps ns)))
        (mapv (partial mapv #(if (< 9 %) 0 %)) ns)))))

(defn part-1 [input]
  (->> (parse input)
       (iterate step)
       next
       (map (comp count (partial filter #{0}) flatten))
       (take 100)
       (reduce +)))

(comment

 (part-1 input)                                             ;; 1644
 )

(defn part-2 [input]
  (->> (parse input)
       (iterate step)
       (map (comp count (partial filter #{0}) flatten))
       (take-while (partial > 100))
       count))

(comment

 (part-2 input)                                             ;; 229
 )
