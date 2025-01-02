(ns aoc2024.day14
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

;;; day 14: Restroom Redoubt

(defn parse-input
  "Returns input as map:
  {:robots ({:x <n> :y <n> :dx <n> :dy <n>}, ...)
   :width <n>
   :height <n>}."
  []
  (let [robots (->> (util/get-input *ns*)
                    (str/split-lines)
                    (map (comp (partial zipmap [:x :y :dx :dy])
                               (partial mapv parse-long)
                               (partial re-seq #"-?\d+"))))]
    {:robots robots
     :width (->> robots (map :x) (reduce max) inc)
     :height (->> robots (map :y) (reduce max) inc)}))

;; part 1

(defn move
  "Returns x moved i times by dx, wrapped at w.
  Or mathematically speaking: (x + i * dx) % w."
  [x dx i w]
  (-> i (* dx) (+ x) (mod w)))

(defn move-robot
  "Moves the given robot i times."
  [w h i {:keys [dx dy] :as r}]
  (-> r
      (update :x move dx i w)
      (update :y move dy i h)))

(defn plot-robots
  [out width height robots]
  (util/plot-maze [height width]
                  (->> robots
                       (map (juxt :x :y))
                       (frequencies))
                  {:fmt (fnil identity \.)
                   :out out
                   :inv? true})
  robots)

(defn calc-safety-factor
  "Returns the safety-factor for the given robots."
  [width height robots]
  (let [x-middle (quot width 2)
        y-middle (quot height 2)
        q-vals (-> {:nw [< <] :ne [> <] :sw [< >] :se [> >]}
                   ; for every quadrant: filter robots, count+calc safety:
                   (update-vals
                     (fn [[x-op y-op]]
                       (->> robots
                            (filter (fn[{:keys [x y]}]
                                      (and (x-op x x-middle)
                                           (y-op y y-middle))))
                            (frequencies)
                            (vals)
                            (reduce +)))))]
    {:quadrants q-vals
     :safety (->> q-vals vals (reduce *))}))

(defn part-1
  []
  (let [{:keys [robots width height]} (parse-input)
        robots-100 (map (partial move-robot width height 100) robots)]
    (plot-robots "var/out-14.txt" width height robots-100)
    (calc-safety-factor width height robots-100)))


;; part 2

(defn print-500
  "Print first 500 seconds so I can look for some clues regarding the tree."
  []
  (let [{:keys [robots width height]} (parse-input)]
    (->> (range 500)
         (run! #(->> robots
                     (map (partial move-robot width height %))
                     (plot-robots (format "var/out-14-%04d.txt" %)
                                  width height))))))

(def part-2-explaination
  "Looking at the first 500 seconds reveals horizontal and vertical
  patterns that seem to repeat themselves:

  horizontal: sec 31, sec 134, sec 237, sec 240, ... -> d = 103
  vertical: sec 68, sec 169, sec 270, sec 371, ... -> d = 101
  sec_h = 103 * i_h + 31
  sec_v = 101 * i_v + 68

  Find iterations so the seconds of horizontal and vertical pattern match:
  sec_h = sec_v
  103 * i_h + 31 = 101 * i_v + 68
  i_h = (101 * i_v + 68 - 31) / 103

  Now we only need to find the lowest pair of [i_v i_h] so that both values
  are (positive) integers. Calculate the seconds value for either iteration
  and we have the solution to part 2.")

(defn part-2
  []
  (let [seconds (->> (range)
                     ; calculate i_h from i_v (like explained above)
                     (map #(-> % (* 101) (+ 68) (- 31) (/ 103)))
                     ; find the first whole i_h:
                     (filter pos-int?)
                     (first)
                     ; calc seconds from the horizontal iteration number:
                     (* 103) (+ 31))
        ; robots-data only needed for plotting, not for the actual solution
        {:keys [robots width height]} (parse-input)
        robots-tree (map (partial move-robot width height seconds) robots)]
    ; let's see the tree!!!
    (plot-robots "var/out-14-tree.txt" width height robots-tree)
    (merge {:seconds seconds}
           (calc-safety-factor width height robots-tree))))
