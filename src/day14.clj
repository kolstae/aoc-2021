(ns day14
  (:require [clojure.string :as str]))

(def small-input "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C")

(def input (str/trim (slurp "resources/day14.txt")))

(defn parse [input]
  (let [[[p] _ is] (partition-by empty? (str/split-lines input))]
    [(seq p) (into {} (map (comp (fn [[a b]] [(seq a) (first b)]) #(str/split % #"\s+->\s+"))) is)]))

(defn grow [input n]
  (let [[p ab->c] (parse input)]
    (->> (partition 2 1 p)
         frequencies
         (iterate #(->> %
                        (mapcat (fn [[[a b] n]] (let [c (ab->c [a b])] [[[a c] n] [[c b] n]])))
                        (reduce (fn [fs [k n]] (update fs k (fnil + 0) n)) {})))
         (drop n)
         first
         (reduce (fn [m [[c] n]] (update m c (fnil + 0) n)) {(last p) 1}))))

(defn part-1 [input]
  (let [fs (vals (grow input 10))]
    (- (apply max fs) (apply min fs))))

(comment

 (part-1 input)                                             ;; 2170
 )

(defn part-2 [input]
  (let [fs (vals (grow input 40))]
    (- (apply max fs) (apply min fs))))

(comment

 (part-2 input)                                             ;; 2422444761283
 )
