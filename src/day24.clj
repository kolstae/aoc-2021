(ns day24
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def small-input "inp w\nadd z w\nmod z 2\ndiv w 2\nadd y w\nmod y 2\ndiv w 2\nadd x w\nmod x 2\ndiv w 2\nmod w 2")

(def input (str/trim (slurp "resources/day24.txt")))

(defn parse [input]
  (->> (str/split-lines input)
       (mapv (comp read-string #(str/join (str \[ % \]))))
       (walk/postwalk #(if (symbol? %) (keyword %) %))))

(def ops {:mul (fn [s k n] (update s k (if (keyword n)
                                         (fn [ns]
                                           (let [ps (for [x ns y (s n)] (* x y))]
                                             [(apply min ps) (apply max ps)]))
                                         (fn [[m M]] (vec (sort [(* m n) (* M n)]))))))
          :add (fn [s k a] (update s k (if (keyword a)
                                         (fn [ns] (mapv + ns (s a)))
                                         (fn [[n m]] [(+ a n) (+ a m)]))))
          :eql (fn [s k a] (update s k (if (keyword a)
                                         (fn [[n m]]
                                           (let [[x y] (s a)]
                                             (cond
                                               (or (< m x) (< y n)) [0 0]
                                               (= n m x y) [1 1]
                                               :else [0 1])))
                                         (fn [[n m]]
                                           (cond
                                             (= n a m) [1 1]
                                             (<= n a m) [0 1]
                                             :else [0 0])))))
          :mod (fn [s k a] (update s k (fn [[n m]]
                                         (if (or (> (- m n) a)
                                                 (> (rem n a) (rem m a)))
                                           [0 (dec a)]
                                           [(rem n a) (rem m a)]))))
          :div (fn [s k a] (update s k (fn [[n m]] (vec (sort [(quot n a) (quot m a)])))))})

(defn compute-range [instr input]
  (loop [state {:w [0 0] :x [0 0] :y [0 0] :z [0 0]}
         instr instr
         input input]
    (if (empty? instr)
      (:z state)
      (let [[op r a] (first instr)]
        (if (= op :inp)
          (recur (assoc state r (first input)) (rest instr) (rest input))
          (recur ((get ops op) state r a) (rest instr) input))))))

(defn solver-f [instr digits]
  (fn rec [fixed-input]
    (let [input (take 14 (concat fixed-input (repeat [1 9])))
          [n m] (compute-range instr input)]
      (cond
        (and (= 14 (count fixed-input))
             (== n 0 m))
        fixed-input

        (or (= 14 (count fixed-input))
            (not (<= n 0 m)))
        nil

        :else (->> digits
                   (map (fn [n] (conj fixed-input [n n])))
                   (some rec))))))

(comment

 (time
  (let [solve (solver-f (parse input) (range 9 0 -1))]
    (str/join (map first (solve [])))))                     ;; part-1 41299994879959
 ;; Elapsed time: 83869.113627 msecs

 (time
  (let [solve (solver-f (parse input) (range 1 10))]
    (str/join (map first (solve [])))))                     ;; part-2 11189561113216
 ;; Elapsed time: 2801.323236 msecs
 )
