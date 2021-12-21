(ns day21
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def small-input "Player 1 starting position: 4\nPlayer 2 starting position: 8")

(def input (str/trim (slurp "resources/day21.txt")))

(defn parse [input]
  (map (comp parse-long last (partial re-find #".+(\d+)$")) (str/split-lines input)))

(defn part-1 [input]
  (let [ps (map vector (repeat 0) (mapv dec (parse input)))]
    (->> (range 1 101)
         cycle
         (partition 3)
         (map (partial apply +))
         (map vector (map (partial * 3) (next (range))))
         (partition 2)
         (reductions (fn [ps nds] (map (fn [[[s p]] [idx d]] (let [n (mod (+ p d) 10)] [[(+ s n 1) n] idx])) ps nds))
                     (map (juxt identity (constantly 0)) ps))
         (mapcat identity)
         (partition 2 1)
         (filter #(some (comp (partial <= 1000) ffirst) %))
         first
         ((juxt (comp ffirst first) (comp second second)))
         (apply *))))

(comment

 (time (part-1 input))                                      ;; 864900
 )

(defn part-2 [input]
  (let [ps (map vector (repeat 0) (mapv dec (parse input)))
        cache (atom {})
        count-wins (fn this [[s1 p1 :as x1] [s2 p2 :as x2]]
                     (cond
                       (<= 21 s1) [1 0]
                       (<= 21 s2) [0 1]
                       :else (if-let [v (get @cache [s1 p1 s2 p2])]
                               v
                               (let [w (reduce (fn [[w1 w2] [x2 x1]] [(+ w1 x1) (+ w2 x2)])
                                               [0 0]
                                               (for [x (range 1 4) y (range 1 4) z (range 1 4) :let [n (mod (+ p1 x y z) 10)]]
                                                 (this [s2 p2] [(+ s1 n 1) n])))]
                                 (swap! cache assoc [s1 p1 s2 p2] w)
                                 w))))]
    (apply max (apply count-wins ps))))

(comment

 (time (part-2 input))                                      ;; 575111835924670
 )
