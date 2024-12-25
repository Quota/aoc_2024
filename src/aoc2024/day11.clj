(ns aoc2024.day11
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

;;; day 11: Plutonian Pebbles

(defn parse-input
  "Returns input as a vector of numbers."
  []
  (->> (util/get-input *ns*)
       str/trim ; remove newline
       (util/parse-numbers #" " [:vec])))

;; part 1

(defn blink
  "Simply applys the rules of this day to the given number, then
  conj's the result to the given vector v and returns that."
  [v n]
  (cond
    (zero? n) (conj v 1)
    (-> n str .length even?) (let [s (str n)
                                   m (-> s .length (quot 2))]
                               (conj v
                                     (parse-long (subs s 0 m))
                                     (parse-long (subs s m))))
    :else (conj v (* n 2024))))

(defn part-1
  []
  (->> (parse-input)
       (iterate #(reduce blink [] %))
       (take 26)
       last
       count))

;; part 2

(def blink-then-count
  "Function of [depth n], applys the rules of this day to the given
  number depth times, then returns the count of the result."
  (memoize
    (fn [depth n]
      (let [depth (max 0 (dec depth))]
        (cond
          ; 0 -> [1]
          (zero? n)
          (if (zero? depth)
            1
            (blink-then-count depth 1))
          ; xy -> [x y]
          (-> n str .length even?)
          (let [n-as-string (str n)
                middle (-> n-as-string .length (quot 2))
                left (parse-long (subs n-as-string 0 middle))
                right (parse-long (subs n-as-string middle))]
            (if (zero? depth)
              2
              (+ (blink-then-count depth left)
                 (blink-then-count depth right))))
          ; n -> [2024 * n]
          :else
          (if (zero? depth)
            1
            (blink-then-count depth (* n 2024))))))))

(defn part-2
  []
  (time
    (->> (parse-input)
         (map #(blink-then-count 75 %))
         (reduce +))))
