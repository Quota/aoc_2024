(ns aoc2024.day16
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]
    [aoc2024.dijkstra :as dijkstra]))

;;; day 16: Reindeer Maze

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (mapv vec)))

(defn rotate
  "Returns the given vec rotated clockwse (:cw) or counterclockwise (:ccw)."
  [[x y] dir]
  (case dir
    :cw [y (- x)]
    :ccw [(- y) x]))

(defn get-neighbors
  "Returns a vec with the neighbors of the given node.
  Neighbors are if a step forward is ok (space, start or end)
  as well if turning clockwise and counterclockwise."
  [{:keys [maze] :as data} [node-rc diff-rc]]
  (if (= \E (get-in maze node-rc))
    []
    (let [step-rc (mapv + node-rc diff-rc)
          rot-cw-drc (rotate diff-rc :cw)
          rot-ccw-drc (rotate diff-rc :ccw) ]
      (cond-> []
        ; turn counterclockwise and check next step
        (#{\. \S \E} (get-in maze (mapv + node-rc rot-ccw-drc)))
        (conj [node-rc rot-ccw-drc])
        ; turn clockwise and check next step
        (#{\. \S \E} (get-in maze (mapv + node-rc rot-cw-drc)))
        (conj [node-rc rot-cw-drc])
        ; check next step
        (#{\. \S \E} (get-in maze step-rc))
        (conj [step-rc diff-rc])
        ))))

(defn calc-costs
  [data [curr-rc curr-drc :as curr] [nbr-rc nbr-drc :as nbr]]
  (cond
      (= curr nbr) 0 ; same nodes -> no cost
      (= curr-rc nbr-rc) 1000 ; same rc -> different direction -> 1000
      :otherwise 1)) ; same direction -> different rc -> 1

(defn plot
  [maze path]
  (let [; make path into map with only the last dir-value
        path (-> (group-by first path)
                 (update-vals (fn[x] (->> x (map second) last))))]
    (doseq [r (-> maze count range)
            c (-> maze count range)
            :let [rc [r c]]]
      (if (and (pos? r) (zero? c))
        (println))
      (let [v (get-in maze rc)]
        (if (#{\S \E \# \X} v)
          (print v)
          (print 
            (case (path rc)
              [0 1] \> [0 -1] \<
              [1 0] \v [-1 0] \^
              \.)))))))

(defn count-steps-and-turns
  "Returns a map {:turns <n> :steps <n>} with the turns and steps of `path`."
  [path]
  (->> path
       (partition 2 1)
       (reduce (fn[a [[xrc xdrc] [yrc ydrc]]]
                 (cond-> a
                   (not= xrc yrc) (update :steps inc)
                   (not= xdrc ydrc) (update :turns inc)))
               {:turns 0 :steps 0})))

;; part 1

(defn part-1
  []
  (let [maze (parse-input)
        se-rc (- (count maze) 2) ; row/col of start/end
        data (merge {:maze maze}
                    (dijkstra/init-data [[se-rc 1] [0 1]]))
        dij-res (dijkstra/run data get-neighbors calc-costs)
        end-costs (->> (:abs-costs dij-res)
                       (filter (fn[[[rc _] v]] (= rc [1 se-rc]))))
        best-end (->> end-costs (sort-by second) ffirst)
        dij-path (dijkstra/get-path dij-res best-end)]
    (spit "var/out-16.txt"
          (with-out-str
            (plot maze dij-path)))
    (merge {:end-costs end-costs}
           (count-steps-and-turns dij-path))))

;; part 2

(defn part-2
  []
  (let [maze (parse-input)
        se-rc (- (count maze) 2)
        data (merge {:maze maze}
                    (dijkstra/init-data [[se-rc 1] [0 1]]))
        dij-res (dijkstra/run data get-neighbors calc-costs)
        dij-path (dijkstra/get-path dij-res [[1 se-rc][-1 0]])]
    (spit "var/out-16.txt"
          (with-out-str
            (plot maze dij-path)))
    ; todo: find all different paths with the same minimum score
    (->> dij-path
         (map first)
         set
         count)))
