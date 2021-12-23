(ns day23
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def small-input "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########")

(def input (str/trim (slurp "resources/day23.txt")))

(defn parse [input]
  (mapv vec (butlast (next (str/split-lines input)))))

(def cost {\A 1 \B 10 \C 100 \D 1000})
(def bot-idx {\A 2 \B 4 \C 6 \D 8})

(defn done? [bs] (every? (fn [[k vs]] (apply = k vs)) bs))
(defn move-from? [b bs] (some (fn [v] (and v (not= b v))) bs))
(defn move-to? [b bs] (every? (fn [v] (or (not v) (= b v))) bs))

(defn clear-path? [bi ti hw]
  (every? (fn [i] (nil? (bot-idx (get hw i))))
          (if (< bi ti)
            (range (inc bi) ti)
            (range (inc ti) bi))))

(defn solve [lines]
  (let [s (into {} (mapcat
                    (fn [y l] (keep-indexed (fn [x c] (when (#{\. \A \B \C \D} c) [[y x] c])) l))
                    (range) lines))
        y-to (inc (apply max (map first (keys s))))
        start [(into {} (map (juxt identity (fn [c] (mapv #(get s [% (inc (bot-idx c))]) (range 1 y-to))))) (keys cost))
               (reduce (fn [v i] (assoc v i :x)) (vec (repeat 11 nil)) (vals bot-idx))]

        cache (atom {})
        solver (fn this [[bm hw :as s]]
                 (if (done? bm)
                   0
                   (if-let [cv (@cache s)]
                     cv
                     (if-let [[cost s'] (first
                                         (for [[i b] (map-indexed vector hw)
                                               :when (char? b)
                                               :let [bs (bm b)]
                                               :when (move-to? b bs)
                                               :let [bi (bot-idx b)]
                                               :when (clear-path? bi i hw)]
                                           (let [[di] (last (remove second (map-indexed vector bs)))]
                                             [(* (cost b) (+ 1 di (Math/abs ^long (- bi i))))
                                              [(assoc-in bm [b di] b) (assoc hw i nil)]])))]
                       (+ cost (this s'))
                       (let [cost (reduce
                                   min
                                   Integer/MAX_VALUE
                                   (for [[b bs] bm
                                         :when (move-from? b bs)
                                         :let [bi (bot-idx b)
                                               [ki c] (first (filter second (map-indexed vector bs)))]
                                         [ti] (remove second (map-indexed vector hw))
                                         :when (clear-path? bi ti hw)]
                                     (let [cost (* (cost c) (+ 1 ki (Math/abs ^long (- ti bi))))]
                                       (+ cost (this [(assoc-in bm [b ki] nil) (assoc hw ti c)])))))]
                         (swap! cache assoc s cost)
                         cost)))))]
    (time (solver start))))

(comment

 (solve (parse input))                                      ;; 15365

 ;; Had to steal almost everything from https://github.com/jonathanpaulson/AdventOfCode/blob/master/2021/23.py :(
 (let [lines (parse input)
       [h t] (split-at 2 lines)
       lines (concat h (map vec (str/split-lines "  #D#C#B#A#\n  #D#B#A#C#")) t)]
   (solve lines))                                           ;; 52055
 )
