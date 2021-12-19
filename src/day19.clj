(ns day19
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def small-input "--- scanner 0 ---\n404,-588,-901\n528,-643,409\n-838,591,734\n390,-675,-793\n-537,-823,-458\n-485,-357,347\n-345,-311,381\n-661,-816,-575\n-876,649,763\n-618,-824,-621\n553,345,-567\n474,580,667\n-447,-329,318\n-584,868,-557\n544,-627,-890\n564,392,-477\n455,729,728\n-892,524,684\n-689,845,-530\n423,-701,434\n7,-33,-71\n630,319,-379\n443,580,662\n-789,900,-551\n459,-707,401\n\n--- scanner 1 ---\n686,422,578\n605,423,415\n515,917,-361\n-336,658,858\n95,138,22\n-476,619,847\n-340,-569,-846\n567,-361,727\n-460,603,-452\n669,-402,600\n729,430,532\n-500,-761,534\n-322,571,750\n-466,-666,-811\n-429,-592,574\n-355,545,-477\n703,-491,-529\n-328,-685,520\n413,935,-424\n-391,539,-444\n586,-435,557\n-364,-763,-893\n807,-499,-711\n755,-354,-619\n553,889,-390\n\n--- scanner 2 ---\n649,640,665\n682,-795,504\n-784,533,-524\n-644,584,-595\n-588,-843,648\n-30,6,44\n-674,560,763\n500,723,-460\n609,671,-379\n-555,-800,653\n-675,-892,-343\n697,-426,-610\n578,704,681\n493,664,-388\n-671,-858,530\n-667,343,800\n571,-461,-707\n-138,-166,112\n-889,563,-600\n646,-828,498\n640,759,510\n-630,509,768\n-681,-892,-333\n673,-379,-804\n-742,-814,-386\n577,-820,562\n\n--- scanner 3 ---\n-589,542,597\n605,-692,669\n-500,565,-823\n-660,373,557\n-458,-679,-417\n-488,449,543\n-626,468,-788\n338,-750,-386\n528,-832,-391\n562,-778,733\n-938,-730,414\n543,643,-506\n-524,371,-870\n407,773,750\n-104,29,83\n378,-903,-323\n-778,-728,485\n426,699,580\n-438,-605,-362\n-469,-447,-387\n509,732,623\n647,635,-688\n-868,-804,481\n614,-800,639\n595,780,-596\n\n--- scanner 4 ---\n727,592,562\n-293,-554,779\n441,611,-461\n-714,465,-776\n-743,427,-804\n-660,-479,-426\n832,-632,460\n927,-485,-438\n408,393,-506\n466,436,-512\n110,16,151\n-258,-428,682\n-393,719,612\n-211,-452,876\n808,-476,-593\n-575,615,604\n-485,667,467\n-680,325,-822\n-627,-443,-432\n872,-547,-609\n833,512,582\n807,604,487\n839,-516,451\n891,-625,532\n-652,-548,-490\n30,-46,-14")

(def input (str/trim (slurp "resources/day19.txt")))

(defn parse [input]
  (let [ss (take-nth 2 (partition-by empty? (str/split-lines input)))]
    (map (fn [[n & cs]]
           [(keyword (str/join "-" (rest (re-find #"(scanner) (\d+)" n))))
            (mapv (comp (partial mapv parse-long) #(str/split % #",")) cs)])
         ss)))

(def signs (vec (for [x [1 -1] y [1 -1] z [1 -1]] [x y z])))
(def pos (vec (for [x (range 3) y (range 3) z (range 3) :when (distinct? x y z)] [x y z])))

(defn permutations [s]
  (let [signed (map (fn [sign] (mapv (partial mapv * sign) s)) signs)]
    (mapcat (fn [[px py pz]] (mapv (partial map (juxt #(get % px) #(get % py) #(get % pz))) signed)) pos)))

(defn align [[n1 s1] [n2 s2]]
  #_(pp/pprint [:align n1 n2])
  (when-let [[diff perm] (->> (permutations s2)
                              (keep (fn [perm]
                                      (let [c->diff (into {} (map (fn [c] [c (map #(mapv - % c) s1)])) perm)
                                            [diff n] (apply max-key val (frequencies (mapcat val c->diff)))]
                                        (when (>= n 12) [diff perm]))))
                              first)]
    [n2 (map (partial mapv + diff) perm) diff]))

(defn align-all [[s1 & ss]]
  (loop [ss ss q [(conj s1 [0 0 0])] done []]
    (if (and (seq ss) (seq q))
      (let [current (peek q)
            #_(pp/pprint [:current current])
            matches (keep (partial align current) ss)]
        #_(pp/pprint [(pop q) matches])
        (recur (remove (comp (set (map first matches)) first) ss)
               (into (pop q) matches)
               (conj done current)))
      (into done q))))

(defn part-1 [input]
  (->> (parse input)
       align-all
       (mapcat second)
       distinct
       count))

(comment

 (time (part-1 input))                                      ;; 383
 )

(defn part-2 [input]
  (let [diffs (map last (align-all (parse input)))]
    (reduce max
            (for [[as bs] (map #(split-at % diffs) (range 1 (count diffs))) b bs
                  :let [a (last as)]]
              (reduce + (map (comp #(Math/abs ^long %) (partial -)) a b))))))

(comment

 (time (part-2 input))                                      ;; 9854
 )
