(ns aoc2024.day04
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

;;; day 1: Ceres Search

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input 4)
       (str/split-lines)
       (vec)))

(defn letter
  [^clojure.lang.PersistentVector input r c]
  (if
    (or (< r 0) (>= r (.size input)) (< c 0))
    nil
    (let [^String s (get input r)]
      (if (>= c (.length s))
        nil
        (.charAt s c)))))

(defn get-grid
  "Returns a seq of pairs [n n] describing the whole input grid."
  [input]
  (for [r (range (count input))
        c (range (count (get input 0)))]
    [r c]))

(def DIRECTIONS
  "8 vecs (with pairs of -1/0/1) pointing into 8 directions.
  Example: [1 0] (=east), [-1 -1] (=south-west).
  There is no [0 0]."
  (for [r (range -1 2) c (range -1 2) :when (not (and (zero? r) (zero? c)))]
    [r c]))

(defn xmas?
  [input [r c] [rd cd]]
  (and ;(= \X (letter input r c)) -> dont need to check this again
       (= \M (letter input (+ r rd) (+ c cd)))
       (= \A (letter input (+ r (* 2 rd)) (+ c (* 2 cd))))
       (= \S (letter input (+ r (* 3 rd)) (+ c (* 3 cd))))))

(defn count-xmas
  [input [r c :as rc]]
  (if (= \X (letter input r c))
    (->> DIRECTIONS
         (filter (fn[rc-dir] (xmas? input rc rc-dir)))
         count)
    0))

;; part 1

(defn part-1
  []
  (let [input (parse-input)]
    (reduce (fn[counter rc]
              (+ counter (count-xmas input rc)))
            0
            (get-grid input))))

;; part 2

(def X-MAS
  {\M [[-1 -1] [-1 1]]
   ;\A [[0 0] [0 0]]
   \S [[1 -1] [1 1]]})

; rotation 90
; |x'| = |0 -1| * |x|    |0x -1y|
; |y'|   |1  0|   |y| =  |1x +0y|

(defn *mv
  "Multiply matrix and vector.
  Matrix must be a vec of row vecs."
  [[r0 r1] v]
  [(apply + (map * r0 v))
   (apply + (map * r1 v))])

(defn rot90
  [v]
  (*mv [[0 -1][1 0]] v))

(def X-MAS-4-ROTATIONS
  (take 4 (iterate (fn[x] (update-vals x #(map rot90 %))) X-MAS)))

(defn check-x-mas-pattern-at
  "returns if input at [r c] contains the given x-mas pattern"
  [input [r c] x-mas]
  (every? (fn [[l rcs]]
            (every? (fn [[r1 c1]]
                      (= l (letter input (+ r r1) (+ c c1))))
                    rcs))
          x-mas))

(defn check-any-x-mas-at
  "returns if input at `rc` contains x-MAS (any rotation of it)"
  [input rc]
  (some
    (fn[x-mas] (check-x-mas-pattern-at input rc x-mas))
    X-MAS-4-ROTATIONS))

(defn part-2
  []
  (let [input (parse-input)]
    (->> (get-grid input)
         ; find all A's in the input
         (filter (fn[[r c]] (= \A (letter input r c))))
         ; around every A check for the 4 rotations of x-MAS
         (filter #(check-any-x-mas-at input %))
         count)))

(comment

  ; alternative idea: coordinate->letter
  (def x-mas3
    {[-1 -1] \M, [-1 1] \M,
     ;[0 0] \A,
     [1 -1] \S, [1 1] \S
     })

  (take 5 (iterate rot90 [1 3]))

  (update-keys x-mas3 rot90)
  (take 2 (iterate #(update-keys % rot90) x-mas3))

  (update-vals X-MAS (partial map rot90))
  (take 2 (iterate #(update-vals % (partial map rot90)) X-MAS))

  (check-any-x-mas-at input [1 2])

)
