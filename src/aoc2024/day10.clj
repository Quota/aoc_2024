(ns aoc2024.day10
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

;;; day 10: Hoof It

(defn parse-input
  "Returns input as map as follows:
  {:island <vec of vec of longs>
   :size <long>
   :trailheads <seq of [r c]>}"
  []
  (let [data (->> (util/get-input *ns*)
                  (str/split-lines)
                  (util/parse-numbers #"" [:vec]))
        trailheads (for [r (range (count data))
                         c (range (count (first data)))
                         :let [rc [r c]]
                         :when (zero? (or (get-in data rc) -9))]
                     rc)]
    {:island data
     :size (count data)
     :trailheads trailheads}))

;; part 1

(defn get-neighbors
  "Returns a seq of the neighbors of the given row/col, or nil if it
  doesn't have neighbors."
  [{:keys [island size]} [r c :as rc]]
  (let [next-level (inc (or (get-in island rc) -9))]
    (->> rc'
         (for [[r' c' :as rc'] [[(dec r) c][(inc r) c][r (dec c)][r (inc c)]]
               :when (and (< -1 r' size)
                          (< -1 c' size)
                          (= next-level (get-in island rc')))])
         seq)))

(defn find-trails
  "Return a list of trails from rc to 9's."
  [{:keys [island size] :as data} rc]
  (if (= 9 (get-in island rc))
    [[rc]]
    (some->> rc
             ; get-neighbors creates: [rc1 rc2 ...]
             (get-neighbors data)
             ; mapcat find-trails: [[..t1_1..] [..t1_2..] .. [..t2_1..] ..]
             (mapcat #(find-trails data %))
             ; map cons: [[rc1..t1_1..] [rc1..t1_2..] ...]
             (map #(cons rc %)))))

(defn part-1
  []
  (let [{:keys [trailheads] :as data} (parse-input)]
    (->> trailheads
         (map #(find-trails data %))
         ; need the num of (distinct) trail ends
         (map #(->> % (map last) distinct count))
         (reduce +))))

;; part 2

(defn part-2
  []
  (let [{:keys [trailheads] :as data} (parse-input)]
    (->> trailheads
         (map #(find-trails data %))
         (map count)
         (reduce +))))
