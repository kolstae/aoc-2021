(ns day10
  (:require [clojure.string :as str]))

(def small-input "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]\n")

(def input (str/trim (slurp "resources/day10.txt")))

(defn parse [input]
  (str/split-lines input))

(def open->close {\< \>, \( \), \{ \}, \[ \]})

(def c->points {\) 3, \] 57, \} 1197, \> 25137})

(defn parse-chunks [s]
  (loop [stack [] [c & more] s]
    (if c
      (if-let [x (open->close c)]
        (recur (conj stack x) more)
        (if (= c (peek stack))
          (recur (pop stack) more)
          c))
      (reverse stack))))

(defn part-1 [input]
  (->> (parse input)
       (keep parse-chunks)
       (filter char?)
       frequencies
       (map (fn [[c n]] (* (c->points c) n)))
       (reduce +)))

(comment

 (part-1 input)                                             ;; 392139
 )

(def c->points2 {\) 1, \] 2, \} 3, \> 4})

(defn middle [coll]
  (when (seq coll)
    (nth coll (/ (count coll) 2))))

(defn part-2 [input]
  (->> (parse input)
       (keep parse-chunks)
       (filter coll?)
       (map #(reduce (fn [n c] (+ (* n 5) (c->points2 c))) 0 %))
       sort
       middle))

(comment

 (part-2 input)                                             ;; 4001832844
 )
