(ns day06
  (:require [clojure.string :as str]))

(def small-input "3,4,3,1,2")

(def input (slurp "resources/day06.txt"))

(defn parse [input]
  (map parse-long (str/split (str/trim input) #",")))

(defn bread [fs days]
  (->> (parse fs)
       frequencies
       (reduce (partial apply assoc) (vec (repeat 9 0)))
       (iterate (fn [v] (-> (subvec v 1)
                            (conj (first v))
                            (update 6 + (first v)))))
       (drop days)
       first
       (reduce +)))

(defn part-1 [input]
  (bread input 80))

(comment

 (part-1 input)                                             ;; 379114
 )

(defn part-2 [input]
  (bread input 256))

(comment

 (part-2 input)                                             ;; 1702631502303
 )
