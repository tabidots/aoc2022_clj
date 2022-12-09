(ns tabidots.day08
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 8: Treetop Tree House")

(def forest
  (-> "../resources/day08.txt" io/resource io/reader line-seq))

(defn make-grid
  [input]
  (mapv #(mapv read-string (re-seq #"\d" %)) input))

(defn viewing-distance
  ;; not elegant but more readable than trying to roll my own "inclusive take-while"
  [self heights]
  (loop [distance       0
         [this & those] heights]
    (cond
      (nil? this)    distance
      (>= this self) (inc distance)
      :else          (recur (inc distance) those))))

(defn tree-stats
  [grid num-cols num-rows [y x]]
  (let [self           (get-in grid [y x])
        my-row         (grid y)
        my-col         (mapv #(nth % x) grid)
        w              (reverse (subvec my-row 0 x))
        n              (reverse (subvec my-col 0 y))
        e              (subvec my-row (inc x) num-cols)
        s              (subvec my-col (inc y) num-rows)
        self-is-taller (partial > self)]
    {:visible?     (some (fn [trees-in-direction]
                           (every? self-is-taller trees-in-direction))
                         [w n e s])
     :scenic-score (apply * (map (partial viewing-distance self) [w n e s]))}))

(defn solve []
  (let [grid     (make-grid forest)
        num-cols (count (first grid))
        num-rows (count grid)
        stats    (for [y (range num-rows)
                       x (range num-cols)]
                   (tree-stats grid num-cols num-rows [y x]))]
    {:part-1 (count (filter :visible? stats))
     :part-2 (apply max (map :scenic-score stats))}))
