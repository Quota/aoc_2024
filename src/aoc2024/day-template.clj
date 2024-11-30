(ns aoc2024.day-template
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

;;; day 1: template

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input 1)
       (str/split-lines)))

;; part 1

(defn part-1
  []
  (->> (parse-input)
       count))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       count))
