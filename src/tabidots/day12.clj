(ns tabidots.day12
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :refer [index-of]]
            [clojure.data.priority-map :refer [priority-map]]))

(comment
 "Day 12: Hill Climbing Algorithm")

(def sample-input
  (-> "../resources/day12_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day12.txt" io/resource io/reader line-seq))

(defn char-to-int
  "Start = 0, end = 25, a-z = 0-25."
  [ch]
  (case ch \S 0 \E 25
    (- (int ch) 97)))

(defn parse
  [input]
  (loop [[[y xs :as this] & those] (map-indexed vector input)
         grid [] start nil goal nil]
    (if-not this
      {:grid grid :start start :goal goal}
      (let [start' (or (some->> (index-of xs "S") (conj [y])) start)
            goal'  (or (some->> (index-of xs "E") (conj [y])) goal)]
        (recur those
          (conj grid (mapv char-to-int (seq xs)))
          start'
          goal')))))

(defn bfs ;; Adapted from my old Project Euler #82 solution and simplified
  "Args: m                     -> the field (matrix) in which to search
        config {:start-coords  -> vector of the starting position [y x]
                :goal?         -> test if current node meets a terminating condition
                :neighbors-fn  -> given node, return all possible neighbors (not nec. existing or unseen)"
  [m config]
  (let [{:keys [start-coords goal? neighbors-fn]} config]
    (loop [stack      (priority-map start-coords 0)
           closed-set {}]
      (let [[coords cost :as current] (peek stack)]
        (cond
          (nil? current) nil ; No path, thanks for playing
          (goal? coords) cost
          :else
          (let [neighbors (into {}
                                (for [neighbor (neighbors-fn coords)
                                      :when (not (contains? closed-set neighbor))]
                                  {neighbor (inc cost)}))]
            (recur (merge-with min (pop stack) neighbors)
              (conj closed-set current))))))))

(defn valid-neighbors
  [grid [y x]]
  (filter (fn [[ny nx]]
            (let [current (get-in grid [y x])
                  nbor    (get-in grid [ny nx] nil)]
              (when nbor
                (<= (- nbor current) 1))))
          [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]]))

(defn starting-points
  [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (zero? (get-in grid [y x]))]
    [y x]))

(defn solve
  [input]
  (let [{:keys [grid start goal]} (parse input)]
    (->> (for [start' (cons start (starting-points grid))]
           (bfs grid {:start-coords start'
                      :goal?        (partial = goal)
                      :neighbors-fn (partial valid-neighbors grid)}))
         ((juxt first #(apply min (remove nil? %))))
         (zipmap [:part-1 :part-2]))))
