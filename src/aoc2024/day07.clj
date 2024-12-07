(ns aoc2024.day07
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [aoc2024.util :as util]))

;;; day 7: Bridge Repair

(defn parse-input
  "Returns input as seq of vecs with [res (arg1 arg2 ...)]."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (util/parse-numbers #"[ :]+")
       (map (juxt first rest))))

;; part 1

(defn calc
  "Applys the given operators to the given args, left-to-right.
  There must be exactly one less ops than there are args."
  [ops args]
  (->> ops
       (reduce
         (fn[[arg1 arg2 & arg-rest] op]
           (cons (op arg1 arg2) arg-rest))
         args)
       first))

(def op-combos
  "Memoized version of `combo/selections ops n`."
  (memoize (fn[ops n] (combo/selections ops n))))

(defn calcable?
  "Checks if any combination of the given ops can solve the term
  res = ops x args. Returns res if true, otherwise nil."
  [ops [res args]]
  (some
    (fn[ops] (if (= res (calc ops args)) res))
    (op-combos ops (dec (count args)))))

(defn part-1
  []
  (time
    (->> (parse-input)
         (keep #(calcable? [* +] %))
         (apply +))))

;; part 2

(defn ||
  "Third operator: concatinate two numbers."
  [l r]
  (parse-long (str l r)))

(defn part-2
  []
  ; brute force all combinations, takes about 21s
  (time
    (->> (parse-input)
         (keep #(calcable? [|| * +] %))
         (apply +))))
