(ns day12
  (:require [clojure.string :as str]))

(def smallest-input "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")
(def smaller-input "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc")
(def small-input "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW")

(def input (str/trim (slurp "resources/day12.txt")))

(defn parse [input]
  (->> (str/split-lines input)
       (map #(mapv keyword (str/split % #"-")))
       (reduce (fn [m [k v]] (cond-> m
                               (and (not= :start v) (not= :end k)) (update k conj v)
                               (and (not= :start k) (not= :end v)) (update v conj k)))
               {})))

(defn mk-extend-path [rs pred]
  (fn [path]
    (->> (get path (dec (count path)))
         (get rs)
         (filter (partial pred path))
         (map (partial conj path)))))

(defn find-paths [extend-path]
  (loop [ps [[:start]] done-ps []]
    (if (seq ps)
      (let [res (mapcat extend-path ps)]
        (recur (remove (comp #{:end} last) res)
               (into done-ps (filter (comp #{:end} last)) res)))
      done-ps)))

(defn part-1 [input]
  (let [rs (parse input)
        multi (set (filter (comp #(= % (str/upper-case %)) name) (mapcat flatten rs)))]
    (->> (mk-extend-path rs (fn [path c] (or (multi c) (not-any? #{c} path))))
         find-paths
         count)))

(comment

 (part-1 input)                                             ;; 3510
 )

(defn part-2 [input]
  (let [rs (parse input)
        multi (set (filter (comp #(= % (str/upper-case %)) name) (mapcat flatten rs)))]
    (->> (mk-extend-path rs (fn [path c] (or (multi c)
                                             (not-any? #{c} path)
                                             (apply distinct? (remove multi path)))))
         find-paths
         count)))

(comment

 (part-2 input)                                             ;; 122880
 )
