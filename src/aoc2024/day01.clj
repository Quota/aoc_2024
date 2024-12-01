(ns aoc2024.day01
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

;;; day 1: Historian Hysteria

(defn parse-input
  "Returns left and right columns of input as vector of to long-vectors.
  Example output: [[8 2 5 ...] [7 9 3 ...]]"
  []
  (->> (util/get-input 1)
       (str/split-lines)
       (map #(re-seq #"\d+" %))
       (map #(map parse-long %))
       (apply mapv vector))) ; "transpose matrix"

;; part 1

(defn part-1
  []
  (let [[left right] (parse-input)]
    (->> (map - (sort left) (sort right))
         (map abs)
         (apply +))))

;; part 2

(defn part-2
  []
  (let [[left right] (parse-input)
        right-freqs (frequencies right)]
    (->> left
         (map #(* % (right-freqs % 0)))
         (apply +))))
