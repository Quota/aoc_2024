(ns aoc2024.day03
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

;;; day 3: Mull It Over

(defn parse-input
  "Returns the single-line input as a long string."
  []
  (-> (util/get-input 3)))

(defn sum-up-products
  "Expects a seq of long-pairs and returns the sum of all products.
  Example: ([1 2] [3 4]) -> 14"
  [coll]
  (->> coll
       (map #(apply * %))
       (apply +)))

;; part 1

(defn part-1
  []
  (->> (parse-input)
       ; -> "foo+mul(12,34)bar;mul(what,not)..."
       (re-seq #"mul\((\d+),(\d+)\)")
       ; -> [["mul(12,34)" "12" "34"] ...]
       (map next)
       ; -> [["12" "34"] ...]
       (map #(map parse-long %))
       ; -> [[12 34] ...]
       sum-up-products))

;; part 2

(defn parse-command
  "Returns a map with the command and arguments, if any.
  The :cmd can be :mul (with seq of numbers as :args)
  and :do (with a boolean in :args)."
  [[cmd arg1 arg2]]
  (case cmd
    "do()"
    {:cmd :do :args true}
    "don't()"
    {:cmd :do :args false}
    ; else "mul(...)"
    {:cmd :mul :args [(parse-long arg1) (parse-long arg2)]}))

(defn collect-commands
  "Filters the commands regarding :do and :dont's
  to collect the multiplication arguments."
  [state {:keys [cmd args]}]
  (case cmd
    ; enable/disable collecting
    :do (assoc state :do args)
    ; collect if enabled
    :mul (if (:do state)
           ; :do is true -> add `args` to :args
           (update state :args conj args)
           ; :do is false -> ignore `args`
           state)))

(defn part-2
  []
  (->> (parse-input)
       ; -> "foo+mul(12,34)bar;mul(what]don't()..."
       (re-seq #"mul\((\d+),(\d+)\)|do(?:n't)?\(\)")
       ; -> [["mul(12,34)" "12" "34"] ["do'nt()" nil nil]...]
       (map parse-command)
       ; -> ({:cmd :mul :args [12 34]} {:cmd :dont} ...)
       (reduce collect-commands {:do true :args []})
       ; -> {:do <ignore> :args [[12 34] ...]}
       :args
       ; -> [[12 34] ...]
       sum-up-products))
