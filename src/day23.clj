(ns day23
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def small-input "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########")

(def input (str/trim (slurp "resources/day23.txt")))

(defn parse [input]
  (mapv vec (butlast (next (str/split-lines input)))))

(def cost {\A 1 \B 10 \C 100 \D 1000})
(def dest-x {\A 3 \B 5 \C 7 \D 9})
(def no-stop-x (into #{} (vals dest-x)))

(defn debug [ls]
  (let [ls (if (map? ls)
             (for [y (range 5)] (for [x (range 13)] (get ls [y x] \#)))
             ls)]
    (doseq [l ls]
      (println (str/join l))))
  ls)

(defn distance [[x1 y1] [x2 y2]] (+ (Math/abs ^long (- x1 x2)) (Math/abs ^long (- y1 y2))))
(defn can-move? [m [y x]] (some (comp #{\.} (partial get m)) [[(dec y) x] [y (inc x)] [y (dec x)]]))

(defn part-1 [input]
  )

(comment

 (let [lines (parse small-input)
       ;[h t] (split-at 2 lines)
       ;lines (concat h (map vec (str/split-lines "  #D#C#B#A#\n  #D#B#A#C#")) t)
       s (debug (into {} (mapcat
                          (fn [y l] (keep-indexed (fn [x c] (when (#{\. \A \B \C \D} c) [[y x] c])) l))
                          (range) lines)))
       y-to (inc (apply max (map first (keys s))))
       ->>debug (fn [s] (prn s) s)

       home? (fn [m y x c]
               (when-let [dx (dest-x c)]
                 (and (= x dx)
                      (every? #{c} (map #(get m [% x]) (range (inc y) y-to))))))

       ->dxr (fn [x dx] (if (< x dx) (range (inc x) dx) (range (dec x) dx -1)))

       possible-moves (fn [m [y x c :as a]]
                        (if (= y 0)
                          (let [dx (dest-x c)
                                pp (when (every? #(= \. (get m [y %])) (->dxr x dx))
                                     (keep #(when-let [dc (#{c \.} (get m [% dx]))] [% dx dc]) (range 1 y-to)))]
                            (when (next pp) (map #(vector [y x c] (subvec % 0 2)) (take-last 1 (filter (comp #{\.} last) pp)))))
                          (map #(vector a [0 %])
                               (concat (sequence (comp (take-while #(= \. (get m [0 %]))) (remove no-stop-x)) (range (inc x) 12))
                                       (take-while #(= \. (get m [0 %])) (remove no-stop-x (range (dec x) 1 -1)))))))
       all-possible-moves (fn [m] (->> m
                                       (sequence (comp
                                                  (keep (fn [[[y x] c]]
                                                          (when (and (dest-x c) (not (home? m y x c)))
                                                            [y x c])))
                                                  (filter (partial can-move? m))
                                                  (mapcat (partial possible-moves m))))
                                       (sort-by (fn [[a b]] (distance a b)))
                                       seq))
       move (fn [m [y1 x1 c] d] [(* (cost c) (distance [y1 x1] d)) (-> (assoc m d c) (assoc [y1 x1] \.))])
       solve-r (fn [s]
                 (loop [s s ms (all-possible-moves s) e 0
                        q '() max-e Long/MAX_VALUE]

                   (if-let [m (first ms)]
                     (let [[d s'] (apply move s m)
                           #_(debug s)
                           used-e (+ e d)]
                       (if (>= used-e max-e)
                         (recur {} [] max-e q max-e)
                         (recur s' (all-possible-moves s') used-e (cond-> q (next ms) (conj [s (rest ms) e])) max-e)))
                     (let [max-e (if (->> s
                                          (map (partial apply conj))
                                          (filter (comp cost last))
                                          (every? (partial apply home? s)))
                                   e
                                   max-e)]
                       (if-let [[s ms e] (peek q)]
                         (do #_(prn :q max-e ms) (recur s ms e (pop q) max-e))
                         max-e)))))]
   #_(home? s 2 7 \C)
   (time (solve-r s)))

 (peek (list 5))

 (time (part-1 input))                                      ;; 15365
 )

(defn part-2 [input]
  )

(comment

 (time (part-2 input))                                      ;;
 )
