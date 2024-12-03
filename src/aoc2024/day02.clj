(ns aoc2024.day02
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

;;; day 2: Red-Nosed Reports

(defn parse-input
  "Returns input as seq of vectors containing the report-levels."
  []
  (->> (util/get-input 2)
       (str/split-lines)
       (util/parse-numbers [:vec])))

;; part 1

(defn analyze-level-diff
  "Returns `wrong-change-fn` as long as the level is ok.
  Otherwise shortcuts to `(reduced false)`."
  [wrong-change-fn level]
  (if (or (zero? level)
          (> (abs level) 3)
          (wrong-change-fn level))
    (reduced false)
    wrong-change-fn))

(defn report-valid?
  "Returns if the given report is valid."
  [report]
  (let [diffs (->> report 
                   (partition 2 1)
                   (map #(apply - %)))]
    (reduce analyze-level-diff
            (if (pos? (first diffs)) neg? pos?)
            diffs)))

(defn part-1
  []
  (->> (parse-input)
       (filter report-valid?)
       count))

;; part 2

(defn vec-remove
  "remove element at `pos` in given vector"
  [pos v]
  (cond
    (zero? pos) (subvec v 1)
    (= pos (dec (count v))) (subvec v 0 pos)
    :otherwise (into (subvec v 0 pos) (subvec v (inc pos)))))

(defn report-valid-with-dampener?
  "Returns if the given report or any dampened version of it is valid
  according to `report-valid?`."
  [report]
  (or
    (report-valid? report)
    (some #(report-valid? (vec-remove % report))
          (range (count report)))))

(defn part-2
  []
  (->> (parse-input)
       (filter report-valid-with-dampener?)
       count))
