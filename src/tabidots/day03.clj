(ns tabidots.day03
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :refer [intersection]]))

(comment
 "Day 3: Rucksack Reorganization")

(def sample-input
  (s/split-lines
   "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"))

(def rucksacks
  (-> "../resources/day03.txt" io/resource io/reader line-seq))

(defn compartmentalize
  [rucksack]
  (-> (/ (count rucksack) 2)
      (split-at rucksack)))

(defn priority
  [input]
  (let [ascii (int input)]
    (cond
      (>= ascii 97) (- ascii 96)     ; a-z -> 1-26
      (<= 65 ascii 90) (- ascii 38)  ; A-Z -> 27-52
      :else nil)))                   ; sanity check

(defn priority-of-rucksack
  [rucksack]
  (->> (compartmentalize rucksack)
       (map set)
       (apply intersection)
       (first)
       (priority)))

(defn part-1
  []
  (apply + (map priority-of-rucksack rucksacks)))

(defn find-badge
  [three-rucks]
  (->> (map set three-rucks)
       (apply intersection)
       (first)))

(defn badges
  [input]
  (map find-badge (partition 3 input)))

(badges rucks)
