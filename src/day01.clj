(ns day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def small-input [199 200 208 210 200 207 240 269 260 263])

(def input (->> "day01.txt"
                io/resource
                slurp
                str/split-lines
                (map parse-long)))

(defn part-1 [input]
  (->> input
       (partition 2 1)
       (filter (partial apply <))
       count))

(comment

 (part-1 input)                                             ;; 1532
 )

(defn part-2 [input]
  (->> input
       (partition 3 1)
       (map (partial apply +))
       part-1))

(comment

 (part-2 input)                                             ;; 1571
 )
