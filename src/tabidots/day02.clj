(ns tabidots.day02
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(comment
 "Day 2: Rock Paper Scissors")

(def sample-input
  ["A Y" "B X" "C Z"])

(def strategy-guide
  (-> "../resources/day02.txt" io/resource io/reader line-seq))

(def points
  {:rock 1 :paper 2 :scissors 3 :lose 0 :draw 3 :win 6})

(def rules
  {:rock {:rock :draw :paper :win :scissors :lose
          :draw :rock :win :paper :lose :scissors}
   :paper {:rock :lose :paper :draw :scissors :win
           :lose :rock :draw :paper :win :scissors}
   :scissors {:rock :win :paper :lose :scissors :draw
              :win :rock :lose :paper :draw :scissors}})

(def their-shape
  {"A" :rock "B" :paper "C" :scissors})

(def your-shape
  {"X" :rock "Y" :paper "Z" :scissors})

(defn one-round-part-1
  [input]
  (let [[them you] (s/split input #" ")
        you-play   (your-shape you)
        outcome    (get-in rules [(their-shape them) you-play])]
    (apply + (map points [outcome you-play]))))

(defn part-1 []
  (apply + (map one-round-part-1 strategy-guide)))

(def outcome-mapping
  {"X" :lose "Y" :draw "Z" :win})

(defn one-round-part-2
  [input]
  (let [[them oc] (s/split input #" ")
        outcome   (outcome-mapping oc)
        you-play  (get-in rules [(their-shape them) outcome])]
    (apply + (map points [outcome you-play]))))

(defn part-2 []
  (apply + (map one-round-part-2 strategy-guide)))
