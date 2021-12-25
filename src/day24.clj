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
       ds (range 9 0 -1)]
   #_(monad [1 1 3 0 0 0 6 1 1 7 9 2 6 6])
   (->> (for [a (range 4 0 -1) b ds [c d] [[2 9] [1 8]] e [9] f (range 9 4 -1) [g h] [[9 4] [8 3] [7 2] [6 1]] i [1] j [7] k [9] l [2] m (range 5 0 -1) n [(+ a 5)]
              :let [mz (get (monad [a b c d e f g h i j k l m n]) 'z)]
              :when (zero? mz)]
          (let [n (parse-long (str/join [a b c d e f g h i j k l m n]))]
            n))
        (partition 2 1)
        (take 200)
        (map (juxt first (partial apply -))))
   ;[[1 1 2 9 9 1 1 1 1 7 9 2 1 1] 11586]
   ;[[1 1 2 9 9 1 9 4 1 7 9 4 9 1] 458]
   ;[[1 1 2 9 9 1 9 4 1 7 9 2 6 1] 17]
   ;[[1 1 2 9 9 1 8 3 1 7 9 2 6 1] 17]
   ;[[1 1 2 9 9 1 7 2 1 7 9 2 6 1] 17]
   ;[[1 1 2 9 9 1 6 1 1 7 9 2 6 1] 17]
   #_(loop [digits (vec (repeat 14 9)) p 0 min-z Long/MAX_VALUE]
       (if (= p 14)
         [digits min-z]
         (let [[d mz] (reduce (fn [[i1 z1] [i2 z2]] (if (<= z1 z2) [i1 z1] [i2 z2])) (map (juxt identity #(monad-z (assoc digits p %))) ds))]
           (recur (assoc digits p d) (inc p) mz)))))
 ;;

 (+ 11299994179256 109566999960)
 (->> [11299994179256
       11299983179256
       11299972179256
       11299961179256
       11299894179246
       11299883179246
       11299872179246
       11299861179246
       11299794179236
       11299783179236
       11299772179236
       11299761179236
       11299694179226
       11299683179226
       11299672179226
       11299661179226
       11299594179216
       11299583179216
       11299572179216
       11299561179216]
      (partition 2 1)
      (map (partial apply -)))

 (parse input)                                              ;; 52055
 )
