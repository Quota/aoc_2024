(ns aoc2024.dijkstra
  (:require [clojure.string :as str])
  (:require [clojure.data.priority-map :refer [priority-map]]))

; dijkstra algorithm

(defn init-data
  "Returns the initial data-map for the dijkstra algorithm."
  [start-node]
  {; map node->costs: absolute costs so far from start
   :abs-costs {start-node 0}
   ; map node->parent: best parents so far (init: start-node to itself)
   :parents {start-node start-node}
   ; priority map (i.e. map sorted by values (not keys)) node->costs
   ; containing the working set of nodes
   :queue (priority-map start-node 0)})

(defn- dijkstra-reducer
  "Dijkstra logic (part 2).
  Input:
  - curr: current node
  - cost-fn: fn [data curr nbr] returning the costs from curr to nbr
  - data: algorithm data so far, see `run-dijkstra`
  - nbr: neighbor of `curr` to check
  Output:
  - Map like `data`"
  [data cost-fn curr nbr]
  (let [new-nbr-costs (+ ((:abs-costs data) curr)
                     (cost-fn data curr nbr))]
    ; if in queue but new-nbr-costs is better (lower),
    ; or not visited yet (no parent)?
    (if (or (and (contains? (:queue data) nbr)
                 (< new-nbr-costs ((:abs-costs data) nbr)))
            (not (contains? (:parents data) nbr)))
      ; then enqueue (or update queue priority) and
      ; update value and parent
      (-> data
          (update :abs-costs assoc nbr new-nbr-costs)
          (update :parents assoc nbr curr)
          (update :queue assoc nbr new-nbr-costs))
      ; else no changes
      data)))

(defn run
  "Dijkstra logic (part 1).
  Input:
  - data: algorithm data as map, as created by `init-data`
    {:abs-costs {node val, ...}  ; absolute costs so far from start to node
     :parents {node parent, ...} ; best parent so far for node
     :queue {node val, ...}} ; priority map, the working data set of the algo
  - neighbors-fn: fn [data curr] returning list of all neighbors of curr
  - cost-fn: fn [data curr nbr] returning the costs from curr to nbr"
  [data neighbors-fn cost-fn]
  (let [curr (first (peek (:queue data)))
        data-next (reduce #(dijkstra-reducer %1 cost-fn curr %2)
                          (assoc data :queue (pop (:queue data)))
                          (neighbors-fn data curr))]
    (if (empty? (:queue data-next))
      data-next
      (recur data-next neighbors-fn cost-fn))))

; other helpers

(defn get-path
  "Returns the path from start-node to the given end.
  The start-node is the one witout a parent or whose parent is itself.
  Return value is a list of nodes."
  [{:keys [parents]} end-node]
  (loop [path (list end-node)
         visited? #{}]
    (let [curr (first path)
          parent (parents curr)]
      (if (or (nil? parent)
              (= parent curr)
              (visited? curr))
        path
        (recur (conj path parent)
               (conj visited? curr))))))

