(ns day25
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def small-input "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>")

(def input (str/trim (slurp "resources/day25.txt")))

(comment

 (let [ls (str/split-lines input)
       max-y (count ls)
       max-x (count (first ls))
       m (into {} (comp (map-indexed (fn [y l] (keep-indexed (fn [x c] (when-not (= \. c) [[y x] c])) l))) (mapcat identity)) ls)
       move->s (fn [state]
                 (fn [m [[y x :as from] c]]
                   (let [to [y (mod (inc x) max-x)]]
                     (cond-> m (not (get state to)) (-> (dissoc from) (assoc to c))))))
       move-vs (fn [state]
                 (fn [m [[y x :as from] c]]
                   (let [to [(mod (inc y) max-y) x]]
                     (cond-> m (not (get state to)) (-> (dissoc from) (assoc to c))))))
       step (fn [m]
              (let [gm (group-by second m)
                    m (reduce (move->s m) m (gm \>))]
                (reduce (move-vs m) m (gm \v))))]
   (->> (iterate step m)
        (map render-m)
        (partition 2 1)
        (take-while (partial apply not=))
        count
        inc))                                               ;; 579
 )
