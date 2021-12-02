(ns day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def small-input (str/split-lines "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"))

(def input (->> "day02.txt"
                io/resource
                slurp
                str/split-lines))

(def cmd->action {"up" [:depth -]
                  "down" [:depth +]
                  "forward" [:pos +]})

(defn parse [s]
  (let [[[_ cmd n]] (re-seq #"(\w+)\s(\d+)" s)]
    [cmd (parse-long n)]))

(defn part-1 [input]
  (->> input
       (map parse)
       (reduce (fn [m [cmd n]]
                 (let [[k f] (get cmd->action cmd)]
                   (update m k f n)))
               {:depth 0 :pos 0})
       vals
       (reduce *)))

(comment

 (part-1 input)                                             ;; 2036120
 )

(def cmd->action2 {"up" [:aim -]
                   "down" [:aim +]
                   "forward" [:pos + true]})

(defn part-2 [input]
  (->> input
       (map parse)
       (reduce (fn [m [cmd n]]
                 (let [[k f aim?] (get cmd->action2 cmd)]
                   (cond-> (update m k f n)
                     aim? (update :depth + (* n (:aim m))))))
               {:depth 0 :pos 0 :aim 0})
       ((juxt :depth :pos))
       (reduce *)))

(comment

 (part-2 input)                                             ;; 2015547716
 )
