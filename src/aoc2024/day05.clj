(ns aoc2024.day05
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [aoc2024.util :as util]))

;;; day 1: Print Queue

(defn parse-input
  "Returns input as map with a :rules set and a :updates seq."
  []
  (let [[rules updates] (->> (util/get-input 5)
                             (#(str/split % #"\n\n"))
                             (map str/split-lines))]
    {:rules (into #{} (map (partial util/parse-numbers #"\|" [:vec]) rules))
     :updates (map (partial util/parse-numbers #"," [:vec]) updates)}))

;; part 1

(defn rules-broken-by-update
  "Returns seq of rules broken by the given update seq.
  Returns nil if no rule was broken.
  rules: #{\"4|7\", \"4|2\", ...}
  u: [\"4\", \"7\", ...]"
  [rules u]
  (->> (combo/combinations u 2)
       (map (fn[[l r]] [r l]))
       (filter rules)
       seq))

(defn get-center-element
  "Returns the element in the middle of the seq
  (or the left one of the center pair). Uses nth."
  [s]
  (nth s (/ (count s) 2)))

(defn part-1
  []
  (let [{:keys [rules updates]} (parse-input)]
    (->> updates
         (remove (partial rules-broken-by-update rules))
         (map get-center-element)
         (apply +))))

;; part 2

(defn rules-with-left-page
  "Returns all rules which have the given page on the left side."
  [rules page]
  (filter (comp #{page} first) rules))

(defn get-rules-of-left-page-in-update
  "Returns the rules with the given page on the left side which are
  relevant for the given update."
  [rules update-set left-page]
  (->> left-page
       (rules-with-left-page rules)
       ; keep only those whose right side is in `update-set`:
       (filter (comp update-set second))))

(defn get-rules-of-update
  "Returns a seq of all pages and \"their\" rules (i.e. page on left side)
  which are relevant for the given update."
  [rules update-vec]
  (let [update-set (set update-vec)]
    (for [page update-vec]
      {:page page :rules (get-rules-of-left-page-in-update rules update-set page)})))

(defn repair-update
  "Returns the repaired version of the given update."
  [rules update-vec]
  ; trick: sort the pages by count of their rules (page on left side)
  (->> (get-rules-of-update rules update-vec)
       (sort-by (comp count :rules) >)
       (map :page)))

(defn part-2
  []
  (let [{:keys [rules updates]} (parse-input)]
    (->> updates
         (filter (partial rules-broken-by-update rules))
         (map (partial repair-update rules))
         (map get-center-element)
         (apply +))))
