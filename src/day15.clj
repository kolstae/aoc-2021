(ns day15
  (:require [clojure.string :as str]))

(def small-input "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581")

(def input (str/trim (slurp "resources/day15.txt")))

(defn parse [input]
  (mapv (partial mapv (comp #(- % (int \0)) int)) (str/split-lines input)))

(defn shortest-path [ns end]
  (loop [v #{} q {[0 0] 0}]
    (let [[[y x :as s] d] (apply min-key val q)]
      (if (= s end)
        d
        (let [v (conj v s)]
          (recur v (reduce (fn [m p]
                             (if-let [pd (get-in ns p)]
                               (update m p (fnil min Long/MAX_VALUE) (+ d pd))
                               m))
                           (dissoc q s)
                           (for [p [[(inc y) x] [(dec y) x] [y (dec x)] [y (inc x)]] :when (not (v p))] p))))))))

(defn part-1 [input]
  (let [ns (parse input)]
    (shortest-path ns [(dec (count ns)) (dec (count (last ns)))])))

(comment

 (time (part-1 input))                                      ;; 410
 )

(defn part-2 [input]
  (let [ns (parse input)
        ns (vec (mapcat (fn [dy ns]
                          (map (fn [ns]
                                 (vec (mapcat (fn [dx ns] (map #(-> % (+ dx dy) dec (mod 9) inc) ns))
                                              (range)
                                              (repeat 5 ns))))
                               ns))
                        (range)
                        (repeat 5 ns)))]
    (shortest-path ns [(dec (count ns)) (dec (count (last ns)))])))

(comment

 (time (part-2 input))                                      ;; 2809
 )
