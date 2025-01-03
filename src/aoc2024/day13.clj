(ns aoc2024.day13
  (:require
    [clojure.string :as str]
    [aoc2024.matrix :as matrix]
    [aoc2024.util :as util]))

;;; day 13: Claw Contraption

(defn parse-game-cfg
  "Returns three lines as [<matrix of l1 and l2> <vector of l3>].
  Example: [ [[1 2] [3 4]] [5 6] ]"
  [lines]
  (let [[a c b d v w] (->> lines
                           (apply str)
                           (re-seq #"\d+")
                           (map parse-long))]
    [[[a b]
      [c d]]
     [v w]]))

(defn parse-input
  "Returns input as seq of results of `parse-game-cfg`."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (partition-by empty?)
       (take-nth 2)
       (map parse-game-cfg)))

(defn calc-costs
  "Returns the number of button presses for all results."
  [results]
  (->> results
       (map (fn[[a b]] (+ (* 3 a) b)))
       (reduce +)))

;; part 1

(defn part-1
  []
  (->> (parse-input)
       (map (partial apply matrix/solve-sole))
       ; only whole results:
       (filter (partial every? integer?))
       (calc-costs)))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       (map #(update % 1 (partial map + [10000000000000 10000000000000])))
       (map (partial apply matrix/solve-sole))
       ; only whole results:
       (filter (partial every? integer?))
       (calc-costs)))
