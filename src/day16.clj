(ns day16
  (:require [clojure.string :as str]))

(def small-input "8A004A801A8002F478")

(def input (str/trim (slurp "resources/day16.txt")))

(def hex->bit {\0 "0000", \1 "0001", \2 "0010", \3 "0011", \4 "0100", \5 "0101", \6 "0110", \7 "0111", \8 "1000", \9 "1001",
               \A "1010", \B "1011", \C "1100", \D "1101", \E "1110", \F "1111"})

(defn take-until [pred coll]
  (transduce (halt-when pred (fn [r h] (conj r h))) conj [] coll))

(defn bits->n [bs] (Long/parseLong (str/join bs) 2))

(defn parse [input]
  (mapcat hex->bit input))

(defn ->header [bs]
  (when (> (count bs) 6)
    (let [[v msg] (split-at 3 bs)
          [t msg] (split-at 3 msg)]
      [(bits->n v) (bits->n t) msg])))

(defn part-1 [input]
  (loop [msg (parse input) vs []]
    (if-let [[v t msg] (->header msg)]
      (let [[v msg] (if (= 4 t)
                      [v (drop (* 5 (count (take-until (comp #{\0} first) (partition 5 msg)))) msg)]
                      (if (= \0 (first msg))
                        [v (drop 15 (next msg))]
                        [v (drop 11 (next msg))]))]
        (recur msg (conj vs v)))
      (reduce + vs))))

(comment

 (part-1 input)                                             ;; 913
 )

(defn gt [a b] (if (> a b) 1 0))
(defn lt [a b] (if (< a b) 1 0))
(defn eq [a b] (if (= a b) 1 0))

(def t->op {0 +, 1 *, 2 min, 3 max, 5 gt, 6 lt, 7 eq})

(defn read-pks [bs n]
  (loop [bs bs is [] n n]
    (if (and (pos? n) (next bs))
      (let [[_ t bs] (->header bs)
            op (t->op t)
            [i bs] (if (= 4 t)
                     (let [nbs (take-until (comp #{\0} first) (partition 5 bs))]
                       [(bits->n (mapcat next nbs)) (drop (* 5 (count nbs)) bs)])
                     (if (= \0 (first bs))
                       (let [[bs more] (split-at 15 (next bs))
                             len (bits->n bs)
                             [is] (read-pks (take len more) len)]
                         [(apply op is) (drop len more)])
                       (let [[bs more] (split-at 11 (next bs))
                             cnt (bits->n bs)
                             [i bs] (read-pks more cnt)]
                         [(apply op i) bs])))]
        (recur bs (conj is i) (dec n)))
      [is bs])))

(defn part-2 [input]
  (ffirst (read-pks (parse input) 1)))

(comment

 (map part-2 ["C200B40A82" "04005AC33890" "880086C3E88112" "CE00C43D881120" "D8005AC2A8F0" "F600BC2D8F" "9C005AC2F8F0" "9C0141080250320F1802104A08"])

 (part-2 input)                                             ;; 1510977819698
 )
