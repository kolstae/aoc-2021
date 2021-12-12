(ns day12
  (:require [clojure.string :as str]))

(def smallest-input "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")
(def smaller-input "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc")
(def small-input "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW")

(def input (str/trim (slurp "resources/day12.txt")))

(defn parse [input]
  (->> (str/split-lines input)
       (map #(mapv keyword (str/split % #"-")))
       (reduce (fn [m [k v]] (-> m
                                 (update k conj v)
                                 (update v conj k)))
               {})))

(defn mk-extend-path [rs multi twice]
  (fn [{:keys [path seen single]}]
    (let [l (get path (dec (count path)))
          seen (cond-> seen
                 (and (not (multi l))
                      (or (not (twice l))
                          (single l))) (conj l))
          single (cond-> single
                   (twice l) (conj l))]
      (->> (get rs l)
           (remove seen)
           (map (fn [k] {:path (conj path k) :seen seen :single single}))
           (group-by (comp boolean (partial some #{:end}) :path))))))

(defn find-paths [extend-path]
  (loop [ps [{:path [:start] :seen #{} :single #{}}] done-ps []]
    (if (seq ps)
      (let [res (map extend-path ps)]
        (recur (mapcat #(get % false) res)
               (apply conj done-ps (mapcat #(get % true) res))))
      (map :path done-ps))))

(defn part-1 [input]
  (let [rs (parse input)
        multi (set (filter (comp #(= % (str/upper-case %)) name) (mapcat flatten rs)))]
    (->> (mk-extend-path rs multi #{})
         find-paths
         count)))

(comment

 (part-1 input)                                             ;; 3510
 )

(defn part-2 [input]
  (let [rs (parse input)
        multi (set (filter (comp #(= % (str/upper-case %)) name) (mapcat flatten rs)))]
    (->> (mapcat flatten rs)
         (remove (into #{:start :end} multi))
         distinct
         (map hash-set)
         (map (partial mk-extend-path rs multi))
         (mapcat find-paths)
         distinct
         count)))

(comment

 (part-2 input)                                             ;; 122880
 )
