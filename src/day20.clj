(ns day20
  (:require [clojure.string :as str]))

(def small-input "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##\n#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###\n.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.\n.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....\n.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..\n...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....\n..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###")

(def input (str/trim (slurp "resources/day20.txt")))

(defn parse [input]
  (let [[alg _ img] (partition-by empty? (str/split-lines input))]
    [(vec (str/join alg)) [\. (vec img)]]))

(def p->b {\. \0 \# \1})

(defn print-img [[bg img]] (doseq [l img] (println (str/join l))) [bg img])

(defn enhance [alg d [bg img]]
  (let [width (count (first img))]
    [(if (= \. bg) (first alg) (peek alg))
     (into []
           (comp (map (comp (partial get alg) #(Long/parseLong % 2) str/join))
                 (partition-all (+ d width d))
                 (map str/join))
           (for [y (range (- d) (+ d (count img))) x (range (- d) (+ d width))]
             (for [y (range (dec y) (+ 2 y)) x (range (dec x) (+ 2 x))]
               (p->b (get-in img [y x] bg)))))]))

(defn lp-after [[alg img] n]
  (->> (iterate #(->> % (enhance alg 1) (enhance alg 1)) img)
       (drop (/ n 2))
       first
       #_print-img
       second
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
