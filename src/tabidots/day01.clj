(ns tabidots.day01
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 1: Calorie Counting")

(def calorie-counts
  (-> "../resources/day01.txt" io/resource io/reader line-seq))

(defn add-calories
  [coll]
  (if (every? (partial = "") coll)
    nil
    (reduce + (map read-string coll))))

(def elves-by-calories
  (->> (partition-by empty? calorie-counts)
       (map add-calories)
       (remove nil?)))

(defn part-1 []
  (apply max elves-by-calories))

(defn part-2 []
  (->> (sort > elves-by-calories)
       (take 3)
       (apply +)))
