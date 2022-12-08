(ns tabidots.day06
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 6: Tuning Trouble")

(def datastream
  (-> "../resources/day06.txt" io/resource io/reader slurp))

(defn not-distinct?
  [coll]
  (not (apply distinct? coll)))

(defn find-start
  [stream & {:keys [payload]}]
  (when-let [num-chars (case payload
                         :marker 4 :message 14 nil)]
    (->> (partition num-chars 1 stream)
         (take-while not-distinct?)
         (count)          ; this is the index of the start of the sequence
         (+ num-chars)))) ; this is the index of the last char in the sequence

(defn part-1 []
  (find-start datastream :payload :marker))

(defn part-2 []
  (find-start datastream :payload :message))
