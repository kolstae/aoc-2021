(ns day17
  (:require [clojure.string :as str]))

(def small-input "target area: x=20..30, y=-10..-5")

(def input (str/trim (slurp "resources/day17.txt")))

(defn parse [input]
  (->> input (re-find #".*x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)") rest (map parse-long) (partition 2)))

(defn hit-for-fn [[[tx-min tx-max] [ty-min ty-max]]]
  (let [take-when-hit (fn [s]
                        (when (when-let [[[x y]] (last s)]
                                (and (<= tx-min x tx-max) (<= ty-min y ty-max)))
                          s))
        possible? (fn [[[x y] [dx dy]]]
                    (and (or (pos? dy) (>= y ty-min))
                         (cond
                           (pos? dx) (<= x tx-max)
                           (neg? dx) (>= x tx-min)
                           :else (<= tx-min x tx-max))))
        step (fn [[[x y] [dx dy]]]
               [[(+ x dx) (+ y dy)]
                [(cond
                   (pos? dx) (dec dx)
                   (neg? dx) (inc dx)
                   :else dx) (dec dy)]])]
    (fn [v]
      (->> (iterate step [[0 0] v])
           (take-while possible?)
           take-when-hit))))

(defn part-1 [input]
  (let [[[_ tx-max] [ty-min] :as ts] (map sort (parse input))
        hit-for (hit-for-fn ts)
        max-y-for (fn [v]
                    (some->> (hit-for v)
                             (map (comp second first))
                             (apply max)))]
    (reduce max (for [x (range 1 (inc tx-max)) y (range ty-min 1000) :let [my (max-y-for [x y])] :when my] my))))

(comment

 (part-1 input)                                             ;; 8646
 )

(defn part-2 [input]
  (let [[[_ tx-max] [ty-min] :as ts] (map sort (parse input))
        hit-for (hit-for-fn ts)]
    (count (for [x (range 1 (inc tx-max)) y (range ty-min 1000) :when (hit-for [x y])] [x y]))))

(comment

 (part-2 input)                                             ;; 5945
 )
