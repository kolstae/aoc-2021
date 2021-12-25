(ns day24
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def small-input "inp w\nadd z w\nmod z 2\ndiv w 2\nadd y w\nmod y 2\ndiv w 2\nadd x w\nmod x 2\ndiv w 2\nmod w 2")

(def input (str/trim (slurp "resources/day24.txt")))

(defn parse [input]
  (mapv (comp read-string #(str/join (str \( % \)))) (str/split-lines input)))

(comment

 (let [ops {'inp :input
            'add +
            'mul *
            'div (comp long /)
            'mod mod
            'eql #(if (= %1 %2) 1 0)}
       ins (parse input)
       monad (fn [digits]
               (reduce (fn [state op]
                         (let [[o k & args] op
                               f (ops o)
                               args (map #(state % %) args)]
                           (-> (if (= :input f)
                                 (-> (update state :input rest)
                                     (assoc k (first (:input state))))
                                 (apply update state k f args))
                               (assoc :after op))
                           ))
                       {'w 0 'x 0 'y 0 'z 0 :input digits} ins))
       monad-z (fn [ds] (get (monad ds) 'z))
       ds (range 9 0 -1)
       as (range 1 10)]
   #_(->> (for [a [4] b [1] [c d] [[2 9] [1 8]] e [9] f (range 9 4 -1) [g h] [[9 4] [8 3] [7 2] [6 1]] i [8] j (range 7 0 -1) k [(+ j 2)] l [9] m (range 5 0 -1) n [(+ a 5)]
                :let [mz (get (monad [a b c d e f g h i j k l m n]) 'z)]
                :when (zero? mz)]
            (let [n (parse-long (str/join [a b c d e f g h i j k l m n]))]
              n))
          (partition 2 1)
          (take 100)
          (map (juxt first (partial apply -))))             ;; part-1 41299994879959
   (->> (for [a [1] b [1] [c d] [[1 8] #_[2 9]] e as f [5] #_(range 5 10) [g h] [[6 1] #_#_#_[9 4] [8 3] [7 2]] i as j [1] #_(range 1 8) k [(+ j 2)] l as m [1] #_(range 1 6) n [(+ a 5)]
              :let [mz (get (monad [a b c d e f g h i j k l m n]) 'z)]
              :when (zero? mz)]
          (let [n (parse-long (str/join [a b c d e f g h i j k l m n]))]
            n))
        (partition 2 1)
        (take 100)
        (map (juxt first (partial apply -))))               ;; part-2 11189561113216
   #_(loop [digits (vec (repeat 14 9)) p 0 min-z Long/MAX_VALUE]
       (if (= p 14)
         [digits min-z]
         (let [[d mz] (reduce (fn [[i1 z1] [i2 z2]] (if (<= z1 z2) [i1 z1] [i2 z2])) (map (juxt identity #(monad-z (assoc digits p %))) ds))]
           (recur (assoc digits p d) (inc p) mz)))))
 )
