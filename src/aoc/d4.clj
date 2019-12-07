(ns aoc.d4)

(def input [236491 713787])

(defn ordered?
  [s]
  (apply <= (map int s)))

(defn count-with
  [span pred]
  (->> (apply range span)
       (map str)
       (filter ordered?)
       (map (comp set vals frequencies))
       (filter pred)
       count))

(defn part1
  []
  (count-with input #(not= % #{1})))

(defn part2
  []
  (count-with input #(% 2)))
