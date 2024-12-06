(ns aoc2024.day05
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [aoc2024.util :as util]))

;;; day 1: Print Queue

(defn parse-input
  "Returns input as map with a :rules map (page -> page-set)
  and a :updates seq (of page-vecs)."
  []
  (let [[rules _ updates] (->> (util/get-input *ns*)
                               str/split-lines
                               (partition-by empty?))
        rules (->> rules
                   (util/parse-numbers #"\|" [:vec])
                   ; group right pages by their left page:
                   (reduce (fn[acc [left right]]
                             (update acc left util/conj-set right))
                           {}))
        updates (util/parse-numbers #"," [:vec] updates)]
    {:rules rules
     :updates updates}))

(defn get-broken-rules
  "Returns set of rules broken by the given update seq.
  Returns empty set if no rule was broken."
  [rules u]
  (into #{}
        (comp (map reverse)
              (filter (partial get-in rules)))
        (combo/combinations u 2)))

(defn get-center-element
  "Returns the element in the middle of the seq
  (or the left one of the center pair). Uses nth."
  [s]
  (nth s (/ (count s) 2)))

;; part 1

(defn part-1
  []
  (let [{:keys [rules updates]} (parse-input)]
    (transduce
      (comp
        (remove #(seq (get-broken-rules rules %)))
        (map get-center-element))
      +
      updates)))

;; part 2

(defn repair-update
  "Returns the repaired version of the given update."
  [rules update-vec]
  (sort #(get-in rules [%1 %2] -1) update-vec))

(defn part-2
  []
  (let [{:keys [rules updates]} (parse-input)]
    (transduce
      (comp
        (filter #(seq (get-broken-rules rules %)))
        (map #(repair-update rules %))
        (map get-center-element))
      +
      updates)))
