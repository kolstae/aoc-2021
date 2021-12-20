(ns day20
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def small-input "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##\n#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###\n.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.\n.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....\n.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..\n...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....\n..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###")

(def input (str/trim (slurp "resources/day20.txt")))

(defn parse [input]
  (let [[alg _ img] (partition-by empty? (str/split-lines input))]
    [(str/join alg) img]))

(def p->b {\. \0 \# \1})

(defn print-img [img] (doseq [l img] (println (str/join l))) img)

(defn rm-border-trim [img]
  (let [img (remove (comp (partial every? #{\.}) butlast rest) (subvec img 1 (dec (count img))))
        img (->> (apply interleave img)
                 (partition (count img))
                 rest
                 butlast
                 (remove (partial every? #{\.})))]
    (->> (apply interleave img)
         (partition (count img))
         (mapv str/join))))

(defn enhance [alg d img]
  (let [width (count (first img))]
    (->> (for [y (range (- d) (+ d (count img))) x (range (- d) (+ d width))]
           (for [y (range (dec y) (+ 2 y)) x (range (dec x) (+ 2 x))]
             (p->b (get-in img [y x] \.))))
         (map (comp (partial get alg) #(Long/parseLong % 2) str/join))
         (partition-all (+ d width d))
         (mapv str/join))))

(defn lp-after [[alg img] n]
  (->> (iterate #(->> % (enhance alg 9) (enhance alg 0) rm-border-trim) img)
       (drop (/ n 2))
       first
       #_print-img
       (mapcat identity)
       (filter #{\#})
       count))

(defn part-1 [input]
  (lp-after (parse input) 2))

(comment

 (time (part-1 input))                                      ;; 5326
 )

(defn part-2 [input]
  (lp-after (parse input) 50))

(comment

 (time (part-2 input))                                      ;; 17096
 )
