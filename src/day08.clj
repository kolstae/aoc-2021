(ns day08
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def small-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def input (str/trim (slurp "resources/day08.txt")))

(defn parse [input]
  (mapv (fn [s] (mapv #(str/split % #"\s+") (str/split s #"\s*\|\s*")))
        (str/split-lines input)))

(defn part-1 [input]
  (->> (parse input)
       (mapcat (comp #(mapv count %) second))
       (filter #{2 3 4 7})
       count))

(comment

 (part-1 input)                                             ;; 288
 )

(def cnt->digit {2 1, 3 7, 4 4, 7 8})

(defn decode [[ns xs]]
  (let [ns (map #(into #{} %) ns)
        cnt->ns (group-by count ns)
        known (into {} (keep (fn [s] (when-let [n (cnt->digit (count s))] [n s]))) ns)
        known (assoc known 9 (first (filter (partial set/subset? (known 4)) (cnt->ns 6))))
        {[zero] true [six] false} (->> (cnt->ns 6)
                                       (remove (into #{} (vals known)))
                                       (group-by (partial set/subset? (known 1))))
        known (assoc known 6 six 0 zero
                           3 (first (filter (partial set/subset? (known 1)) (cnt->ns 5)))
                           5 (first (filter (partial set/superset? six) (cnt->ns 5))))
        known (assoc known 2 (first (remove (into #{} (vals known)) (cnt->ns 5))))]
    (->> (map #(into #{} %) xs)
         (map (into {} (map (comp vec reverse)) known))
         (reduce (fn [n d] (+ (* 10 n) d))))))

(defn part-2 [input]
  (->> (parse input)
       (map decode)
       (reduce +)))

(comment

 (part-2 input)                                             ;; 940724
 )
