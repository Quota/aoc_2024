(ns aoc2024.day17
  (:require
    [clojure.string :as str]
    [clojure.math :as math]
    [aoc2024.util :as util]))

;;; day 17: Chronospatial Computer

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map #(str/split % #"[: ]+"))
       (reduce
         (fn[res [t a1 a2]]
           (case t
             "Register" (assoc res (keyword (str/lower-case a1)) (parse-long a2))
             "Program" (assoc res :prog (util/parse-numbers #"," [:vec] a1))
             res))
         {:term 1000 
          :ip 0
          :out []})))

(defn combo
  [{:keys [a b c]} opa]
  (case opa
    4 a, 5 b, 6 c,
    opa))

(def opc->name {0 "adv" 1 "bxl" 2 "bst" 3 "jnz" 4 "bxc" 5 "out" 6 "bdv" 7 "cdv"})

(defn adv
  [data a opa]
  (quot a (int (math/pow 2 (combo data opa)))))
  
(defn run
  [{:keys [term ip a b c prog] :as data}]
  (if (zero? term)
    (throw (ex-info "Too many loops" data)))
  (let [opc (get prog ip)
        opa (get prog (inc ip))
        data (-> data 
                 (update :term dec)
                 (update :ip #(+ 2 %)))]
    (if opc
      (do
        ; #(println "### ip:" ip "op:" (opc->name opc) opa "regs:" a b c)
        (recur (case opc
                 ; adv division: A 2^combo -> A
                 0 (assoc data :a (adv data a opa))
                 ; bxl bit-xor: B literal -> B
                 1 (assoc data :b (bit-xor b opa))
                 ; bst module 8: combo % 8 -> B
                 2 (assoc data :b (mod (combo data opa) 8))
                 ; jnz jump: if A!=0 jump to literal
                 3 (if (pos? a) 
                     (assoc data :ip opa)
                     data)
                 ; bxc bit-xor: B C -> B (ignoring opa)
                 4 (assoc data :b (bit-xor b c))
                 ; out output: combo % 8 -> output
                 5 (update data :out conj (mod (combo data opa) 8))
                 ; bdv divison: A 2^combo -> B
                 6 (assoc data :b (adv data a opa))
                 ; cdv divison: A 2^combo -> C
                 7 (assoc data :c (adv data a opa)))))
      data)))

;; part 1

(defn part-1
  []
  (let [data (parse-input)
        res (run data)]
    {:out (->> res :out (str/join ",")) :raw res}))

;; part 2

(defn part-2
  []
  (let [data (parse-input)]
    (reduce
      (fn[_ i]
        (if (zero? (mod i 10000)) (println (format "%,d..." i)))
        (let [{:keys [prog out] :as res} (-> data (assoc :a-start i) (assoc :a i) run)]
          (if (= prog out)
            (reduced res))))
      nil
      (range 0 (int 100000)) ; todo: brute force does not work...
      )))

