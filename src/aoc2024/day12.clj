(ns aoc2024.day12
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [aoc2024.util :as util]))

;;; day 12: Garden Groups

(defn get-neighbors
  "Returns a set with the (four) neighbors of r/c."
  [[r c]]
  #{[(dec r) c][(inc r) c][r (dec c)][r (inc c)]})

(defn parse-region
  "Used by `parse-input` to collect all r/c of one plant."
  [m {:keys [region] :as rd} rc] ; m = garden map, rd = region-data
  (let [plant-val (get-in m rc)
        new-p-nbs (->> (get-neighbors rc)
                       (filter #(= plant-val (get-in m %)))
                       (remove region))]
    (if (seq new-p-nbs)
      ; new neighbors found
      (as-> rd $
          ; add them to region-data
          (reduce #(update %1 :region conj %2)
                  $
                  new-p-nbs)
          ; and continue search with them
          (reduce (partial parse-region m)
                  $
                  new-p-nbs))
      ; no new neighbors: return region-data
      rd)))

(defn parse-input
  "Returns input as vec of region-data maps.
  Example: [{:plant <ch> :region #{rc1, rc2, ...}}, ...]"
  []
  (let [m (->> (util/get-input *ns*)
               (str/split-lines)
               (mapv vec))
        size (count m)]
    (->> (for [r (range size) c (range size)] [r c])
         ; run over all r/c and collect regions
         (reduce (fn[v rc]
                   ; if rc is already part of a region: do nothing
                   (if (seq (filter (fn[{:keys [region]}] (region rc)) v))
                     v
                     ; otherwise: create new region for rc
                     (conj v
                           (parse-region m
                                         {:plant (get-in m rc) :region #{rc}}
                                         rc))))
                 []))))

;; part 1

(defn count-open-sides
  [region rc]
  (->> (get-neighbors rc)
       (remove region)
       count))

(defn get-fence-data
  [region]
  {:a (* (count region))
   :p (->> region
           (map #(count-open-sides region %))
           (reduce +))})

(defn part-1
  []
  (->> (parse-input)
       (map :region)
       (map get-fence-data)
       (map (comp (partial apply *) vals))
       (apply +)))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       count))
