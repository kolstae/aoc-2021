(ns day15
  (:require [clojure.string :as str]))

(def small-input "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581")

(def input (str/trim (slurp "resources/day15.txt")))

(defn parse [input]
  (mapv (partial mapv (comp #(- % (int \0)) int)) (str/split-lines input)))

(defn shortest-path [ns end]
  (loop [dist {[0 0] 0} v #{} q #{[0 0]}]
    (let [s (apply min-key #(get dist % Long/MAX_VALUE) q)
          d (dist s)]
      (if (= s end)
        d
        (let [v (conj v s)
              nds (into []
                        (comp (map (partial mapv + s))
                              (keep (fn [p] (when (not (v p))
                                              (when-let [pd (get-in ns p)]
                                                (when (< (+ d pd) (get dist p Long/MAX_VALUE))
                                                  [p (+ d pd)]))))))
                        [[1 0] [-1 0] [0 -1] [0 1]])]
          (recur (into (dissoc dist s) nds) v (into (disj q s) (map first) nds)))))))

(defn part-1 [input]
  (let [ns (parse input)]
    (shortest-path ns [(dec (count ns)) (dec (count (last ns)))])))

(comment

 (part-1 input)                                             ;; 410
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
