(ns day16
  (:require [clojure.string :as str]))

(def small-input "8A004A801A8002F478")

(def input (str/trim (slurp "resources/day16.txt")))

(def hex->bit {\0 "0000", \1 "0001", \2 "0010", \3 "0011", \4 "0100", \5 "0101", \6 "0110", \7 "0111", \8 "1000", \9 "1001",
               \A "1010", \B "1011", \C "1100", \D "1101", \E "1110", \F "1111"})

(defn bits->n [bs] (Long/parseLong (str/join bs) 2))

(defn parse [input] (mapcat hex->bit input))

(defn ->header [bs]
  (when (> (count bs) 6)
    (let [[v msg] (split-at 3 bs)
          [t msg] (split-at 3 msg)]
      [(bits->n v) (bits->n t) msg])))

(defn gt [a b] (if (> a b) 1 0))
(defn lt [a b] (if (< a b) 1 0))
(defn eq [a b] (if (= a b) 1 0))

(def t->op {0 '+, 1 '*, 2 'min, 3 'max, 5 'gt, 6 'lt, 7 'eq})

(defn read-literal [bs]
  (let [[nbs [t]] (split-with (comp #{\1} first) (partition 5 bs))]
    [(bits->n (mapcat next (concat nbs [t]))) (drop (* 5 (inc (count nbs))) bs)]))

(defn read-op [rdr op bs]
  (let [[is rest] (if (= \0 (first bs))
                    (let [[bs more] (split-at 15 (next bs))
                          len (bits->n bs)]
                      [(first (rdr (take len more) len)) (drop len more)])
                    (let [[bs more] (split-at 11 (next bs))]
                      (rdr more (bits->n bs))))]
    [(into [op] is) rest]))

(defn read-pks [bs n]
  (loop [bs bs is [] n n]
    (if-let [[v t bs] (and (pos? n) (->header bs))]
      (let [[i bs] (if (= 4 t)
                     (read-literal bs)
                     (read-op read-pks (t->op t) bs))]
        (recur bs (conj is (conj [v] i)) (dec n)))
      [is bs])))

(defn unpack-v [[v e]] (if (coll? e) (apply + v (map unpack-v (rest e))) v))

(defn part-1 [input]
  (-> (parse input) (read-pks 1) ffirst unpack-v))

(comment

 (part-1 input)                                             ;; 913
 )

(defn unpack-op [[_ e]] (if (coll? e) (cons (first e) (map unpack-op (rest e))) e))

(defn part-2 [input]
  (-> (parse input) (read-pks 1) ffirst unpack-op eval))

(comment

 (map part-2 ["C200B40A82" "04005AC33890" "880086C3E88112" "CE00C43D881120" "D8005AC2A8F0" "F600BC2D8F" "9C005AC2F8F0" "9C0141080250320F1802104A08"])

 (part-2 input)                                             ;; 1510977819698
 )
