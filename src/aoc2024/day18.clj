(ns aoc2024.day18
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]
    [aoc2024.dijkstra :as dijkstra]))

;;; day 18: RAM Run

(defn parse-input
  "Returns input as {:corrupted [[x y] ...] :end [x-max y-max]}."
  []
  (let [corrupted (->> (util/get-input *ns*)
                    (str/split-lines)
                    (util/parse-numbers #"," [:vec])
                    (reduce (partial conj) []))
        xy-max (->> corrupted seq flatten (apply max))]
    {:corrupted corrupted
     :end [xy-max xy-max]}))

;; part 1

(defn get-neighbors
  [{[xe ye :as end] :end corrupted :corrupted} [x y :as xy]]
  (if (= end xy)
    nil
    (for [[x' y' :as xy] [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
          ;:when (not= end xy) ; end point does not have neighbors
          :when (<= 0 x' xe) ; within memory bounds
          :when (<= 0 y' ye) ; within memory bounds
          :when (nil? (corrupted xy))] ; if not corrupted
      xy)))

(defn part-1
  []
  (let [data (-> (parse-input)
                 (update :corrupted (comp set (partial take 1024)))
                 (merge (dijkstra/init-data [0 0])))
        result (dijkstra/run data get-neighbors (constantly 1))
        path (dijkstra/get-path result (:end data))]
    {:path path
     :steps (dec (count path))}))

;; part 2

(defn part-2
  []
  ; takes about 11s
  (time 
    (let [{:keys [corrupted end] :as data}
          (-> (parse-input)
              (merge (dijkstra/init-data [0 0])))]
      (->> (count corrupted)
           (range 2000)
           (filter (fn[n]
                     (let [corr (subvec corrupted 0 (inc n))]
                       (-> data
                           (assoc :corrupted (set corr))
                           (dijkstra/run get-neighbors (constantly 1))
                           :parents (get end)
                           nil?))))
           first
           corrupted))))
