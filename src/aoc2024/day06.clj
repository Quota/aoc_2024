(ns aoc2024.day06
  (:require
    [clojure.string :as str]
    [clojure.core.async :as async]
    [aoc2024.util :as util])
  (:import
    [java.awt Color]
    [javax.swing JFrame]))

;;; day 6: Guard Gallivant

; use "pseudo" global binding to avoid passing the lab-map everywhere
(defonce ^:dynamic *lab* nil)

(defn parse-input
  "Returns input (laboratory map) as vec of vecs of ., # and ^."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (mapv vec)))

(defn find-guard
  "Returns the location [r c] of the guard in the lab.
  (Used to find the starting location.)"
  []
  (ffirst
    (keep-indexed
      (fn[r row] (seq (keep-indexed (fn[c x] (if (= x \^) [r c])) row)))
      *lab*)))

(def turn-right
  "Map that \"implements\" one direction turn operation (90 deg. cw.)."
  {:up :right, :right :down, :down :left, :left :up})

(defn add-step
  "Returns the location moved by one step into the given direction."
  [[r c] dir]
  (case dir
    :up [(dec r) c]
    :right [r (inc c)]
    :down [(inc r) c]
    :left [r (dec c)]))

(defn guard-move
  "Lets the guard make one move, which is either stepping forward
  or turning around -- or leaving the lab or 'realizing' she's
  entered a loop.
  Will update either:
  - :location and :visited
  - :direction
  - :exit (to the location the guard left the lab) and :visited
  - :loop-at (to the location+direction the loop starts)"
  [{:keys [location direction visited] :as state}]
  ; check for loop
  (if (visited [location direction])
    (assoc state :loop-at [location direction])
    (let [next-loc (add-step location direction)
          object (get-in *lab* next-loc)]
      ; regular move
      (case object
        ; free space -> step forward
        (\. \^) (-> state
                    (assoc :location next-loc)
                    (update :visited conj [location direction]))
        ; obstacle -> turn right 90 deg
        \# (-> state
               (update :direction turn-right))
        ; stepped outside the lab...
        nil (-> state
                (update :visited conj [location direction])
                (assoc :exit next-loc))))))

(defn guard-run
  "Lets the guard run through the lab by calling `guard-move`
  until she left the lab or entered a loop."
  [start-loc]
  (loop [state {:start start-loc
                :location start-loc
                :direction :up
                :visited #{}}]
    (let [next-state (guard-move state)]
      (if-not (or (:exit next-state) (:loop-at next-state))
        (recur next-state)
        next-state))))

;; part 1

(defn part-1
  "Let the guard run and, among other, return how many locations
  she visited in the lab."
  []
  (binding [*lab* (parse-input)]
    (let [{:keys [start exit visited] :as state} (guard-run (find-guard))]
      {:start start
       :exit exit
       :visited-count (count (set (map first visited)))
       :steps (count visited)
       })))

;; part 2

(defn part-2
  "Let the guard run once, then simulate her path through the lab
  again with one obstacle added per simulation, for every location
  she visited, to find the number of obstacles which lead the guard
  into loops."
  []
  ; runs aprox. 10 seconds (or 26 w/o parallel map...)
  (time
    (binding [*lab* (parse-input)]
      (let [start-loc (find-guard)
            visited (->> start-loc guard-run :visited
                         ; just the location, no direction:
                         (map first)
                         ; remove duplicates:
                         set
                         ; can't obfuscate start loc:
                         (#(disj % start-loc)))]
        ; for every visited place: put an obstacle at that location
        ; and call `guard-run` to see if the path loops.
        (->> visited
             (pmap
               (fn[vis-rc]
                 (binding [*lab* (assoc-in *lab* vis-rc \#)]
                   (if-let [loop-at (:loop-at (guard-run start-loc))]
                     {:obstacle vis-rc :loop-at loop-at}))))
             (remove nil?)
             count)))))
