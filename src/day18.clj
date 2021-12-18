(ns day18
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.zip :as zip]))

(def small-input "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")

(def input (str/trim (slurp "resources/day18.txt")))

(defn parse [input]
  (mapv read-string (str/split-lines input)))

(defn do-addition [sns]
  (let [iter-ns (fn [z f] (filter (comp number? zip/node) (take-while #(and % (not (zip/end? %))) (next (iterate f z)))))
        find-ex (fn [z] (->> (iter-ns z zip/next)
                             (map zip/up)
                             distinct
                             (map (juxt identity #(count (zip/path %))))
                             (reduce (fn [a b] (if (>= (second a) (second b)) a b)))))
        sn-split (fn [sn]
                   (if-let [loc (first (filter #(< 9 (zip/node %)) (iter-ns (zip/vector-zip sn) zip/next)))]
                     (zip/root (zip/edit loc (fn [n] [(long (/ n 2)) (long (/ (inc n) 2))])))
                     sn))
        explode (fn [sn]
                  (loop [z (zip/vector-zip sn)]
                    (let [[loc d] (find-ex z)]
                      (if (>= d 4)
                        (let [[a b] (zip/node loc)
                              loc (zip/replace loc 0)
                              loc (if-let [loc (first (iter-ns loc zip/prev))]
                                    (first (iter-ns (zip/edit loc + a) zip/next))
                                    loc)
                              loc (if-let [loc (first (iter-ns loc zip/next))]
                                    (zip/edit loc + b)
                                    loc)]
                              (recur (zip/vector-zip (zip/root loc))))
                        (zip/root loc)))))
        sn-reduce (fn sn-reduce [sn]
                    (loop [sn sn]
                      (let [r (sn-split (explode sn))]
                        (if (not= sn r) (recur r) sn))))]
    (walk/postwalk (fn [x] (if (and (coll? x) (every? number? x)) (apply + (map * [3 2] x)) x))
                   (reduce (comp sn-reduce vector) sns))))

(defn part-1 [input]
  (do-addition (parse input)))

(comment

 (time (part-1 input))                                      ;; 3524
 ;; "Elapsed time: 1312.704442 msecs"
 )

(defn part-2 [input]
  (let [sns (parse input)]
    (reduce max
            (for [[a bs] (map #(split-at % sns) (range 1 (count sns))) b bs]
              (max (do-addition [(last a) b])
                   (do-addition [b (last a)]))))))

(comment

 (time (part-2 input))                                      ;; 4656
 ;; "Elapsed time: 19095.817027 msecs"
 )
