(ns aoc2024.day19
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

;;; day 19: Linen Layout

(defn parse-input
  "Returns input as map {:towels [<str>, ..] :designs [<str>, ..]}."
  []
  (let [[towels _ & designs] (->> (util/get-input *ns*)
                                (str/split-lines))]
    {:towels (str/split towels #", *")
     :designs designs}))

;; part 1

(defn part-1-re
  []
  (let [{:keys [towels designs]} (parse-input)
        t-re (re-pattern (str "^(?:" (str/join "|" towels) ")+$"))]
    (->> designs
         (filter (fn[d] (re-matches t-re d)))
         count)))

(def check-design
  "Tests if `design` can be constructed via combinations of `towels`.
  Uses memoize and (true) recursion via divide and conquer.
  Returns the number of different towel-combinations for the given design."
  (memoize
    (fn
      ; using type hints improves speed 10x - 20x
      [^clojure.lang.PersistentVector towels ^String design]
      (->> towels
           (filter (fn[^String t] (.startsWith design t)))
           (some (fn[^String p]
                   (or
                     (= (.length p) (.length design)) ; final prefix/match
                     (check-design towels
                                   (.substring design (.length p))))))))))

(defn part-1
  []
  (let [{:keys [towels designs]} (parse-input)]
    (->> designs
         (pmap #(check-design towels %))
         (filter identity)
         count)))

;; part 2

(def count-design-combinations
  "Tests if `design` can be constructed via combinations of `towels`.
  Uses memoize and (true) recursion via divide and conquer.
  Returns the number of different towel-combinations for the given design."
  (memoize
    (fn
      ; using type hints improves speed 10x - 20x
      [^clojure.lang.PersistentVector towels ^java.lang.String design]
      (->> towels
           (filter (fn[^java.lang.String t] (.startsWith design t)))
           (map (fn[^String match]
                  (if (= (.length match) (.length design))
                    1
                    (count-design-combinations
                      towels
                      (.substring design (.length match))))))
           (apply +)))))

(defn part-2
  []
  (let [{:keys [towels designs]} (parse-input)]
    (->> designs
         (pmap #(count-design-combinations towels %))
         (apply +))))
