(ns tabidots.day04
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 4: Camp Cleanup")

(def assignments
  (-> "../resources/day04.txt" io/resource io/reader line-seq))

(defn complete-overlap?
  [assignment]
  (let [[elf-a-min elf-a-max elf-b-min elf-b-max] (->> (re-seq #"\d+" assignment)
                                                       (map read-string))]
    (or (and (<= elf-a-min elf-b-min) (>= elf-a-max elf-b-max))
        (and (<= elf-b-min elf-a-min) (>= elf-b-max elf-a-max))
        nil)))

(defn part-1 []
  (count (keep complete-overlap? assignments)))

(defn partial-overlap?
  [assignment]
  (let [[elf-a-min elf-a-max elf-b-min elf-b-max] (->> (re-seq #"\d+" assignment)
                                                       (map read-string))]
    (or (<= elf-a-min elf-b-min elf-a-max)
        (<= elf-a-min elf-b-max elf-a-max)
        (<= elf-b-min elf-a-min elf-b-max)
        (<= elf-b-min elf-a-max elf-b-max)
        nil)))

(defn part-2 []
  (count (keep partial-overlap? assignments)))
