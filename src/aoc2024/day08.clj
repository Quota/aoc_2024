(ns aoc2024.day08
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [aoc2024.util :as util]))

;;; day 8: Resonant Collinearity

(defn parse-input
  "Returns input as a city map with :antennas and :size.
  The :antennas is a map of frequencies to sets of their locations [r c],
  and :size is the number of rows/columns of the city.
  Example: {:antennas {\\A #{[10 20] ...}, ...} :size 12}"
  []
  (let [lines (->> (util/get-input *ns*) (str/split-lines) (mapv vec))
        antennas (reduce-kv
         (fn[acc-outer r row-vec]
           (reduce-kv
             (fn[acc-inner c l]
               (if (not= \. l)
                 (update acc-inner l util/conj-set [r c])
                 acc-inner))
             acc-outer
             row-vec))
         {}
         lines)]
    {:antennas antennas
     :size (count lines)}))

;; part 1

(defn part-1
  []
  (let [{:keys [antennas size]} (parse-input)]
    (->> antennas
         vals
         (mapcat (fn [ants]
                   (->> ; create pairs of all antenna locations:
                        (combo/combinations ants 2)
                        ; calc antinode locations for every antenna-pair:
                        (mapcat (fn[[l1 l2]]
                                  (let [d (map - l2 l1)]
                                    [(map - l1 d) (map + l2 d)])))
                        ; remove antinode-locations outside the city:
                        (filter (fn[[r c]]
                                  (and (< -1 r size) (< -1 c size))))
                        ; make distinct
                        set)))
         ; again, make distinct before finally counting
         set
         count)))

;; part 2

(defn part-2
  []
  (let [{:keys [antennas size]} (parse-input)
        inside? (fn[[r c]]
                  (and (< -1 r size) (< -1 c size)))]
    (->> antennas
         vals
         (mapcat (fn [ants]
                   (->> ; create pairs of all antenna locations:
                        (combo/combinations ants 2)
                        ; calc antinode locations for every antenna-pair:
                        (mapcat (fn[[l1 l2]]
                                  (let [d (map - l2 l1)]
                                    (-> #{}
                                        (into (->> (iterate #(mapv - % d) l1)
                                                   (take-while inside?)))
                                        (into (->> (iterate #(mapv + % d) l2)
                                                   (take-while inside?)))
                                        ))))
                        ; make distinct
                        set)))
         ; again, make distinct before finally counting
         set
         count)))
