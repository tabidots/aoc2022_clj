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

(defn visible?
  [grid num-cols num-rows [y x]]
  (let [self           (get-in grid [y x])
        w              (map #(get-in grid [y %] 0) (range 0 x))
        n              (map #(get-in grid [% x] 0) (range 0 y))
        e              (map #(get-in grid [y %] 0) (range (inc x) num-cols))
        s              (map #(get-in grid [% x] 0) (range (inc y) num-rows))
        self-is-taller (partial > self)]
    (some (fn [trees-in-direction]
            (every? self-is-taller trees-in-direction))
          [w n e s])))

(defn part-1 []
  (let [grid     (make-grid forest)
        num-cols (count (first grid))
        num-rows (count grid)]
    (count
      (for [y (range num-rows)
            x (range num-cols)
            :when (visible? grid num-cols num-rows [y x])]
        true))))

(defn viewing-distance
  ;; not elegant but more readable than trying to roll my own "inclusive take-while"
  [self heights]
  (loop [distance       0
         [this & those] heights]
    (cond
      (nil? this)    distance
      (>= this self) (inc distance)
      :else          (recur (inc distance) those))))

(defn scenic-score
  [grid num-cols num-rows [y x]]
  (let [self (get-in grid [y x])
        w    (reverse (map #(get-in grid [y %] 0) (range 0 x)))
        n    (reverse (map #(get-in grid [% x] 0) (range 0 y)))
        e    (map #(get-in grid [y %] 0) (range (inc x) num-cols))
        s    (map #(get-in grid [% x] 0) (range (inc y) num-rows))]
    (apply * (map (partial viewing-distance self) [w n e s]))))

(defn part-2 []
  (let [grid     (make-grid forest)
        num-cols (count (first grid))
        num-rows (count grid)]
    (apply max
      (for [y (range num-rows)
            x (range num-cols)]
        (scenic-score grid num-cols num-rows [y x])))))
