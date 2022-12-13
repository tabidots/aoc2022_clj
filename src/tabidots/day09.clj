(ns tabidots.day09
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 9: Rope Bridge")

(def motions
  (-> "../resources/day09.txt" io/resource io/reader line-seq))

(defn add-coords
  [path line]
  (let [direction     (re-find #"[RUDL]" line)
        distance      (read-string (re-find #"\d+" line))
        [cur-x cur-y] (peek path)]
    (reduce conj path
      (case direction
        "U" (map #(vector cur-x %) (range (inc cur-y) (inc (+ cur-y distance))))
        "D" (reverse (map #(vector cur-x %) (range (- cur-y distance) cur-y)))
        "L" (reverse (map #(vector % cur-y) (range (- cur-x distance) cur-x)))
        "R" (map #(vector % cur-y) (range (inc cur-x) (inc (+ cur-x distance))))))))

(defn head-path
  [input]
  (reduce add-coords [[0 0]] input))

(defn adjacent?
  [[ax ay] [bx by]]
  (and (<= (Math/abs (- ax bx)) 1)
       (<= (Math/abs (- ay by)) 1)))

(defn two-apart?
  [a b]
  (= 2 (Math/abs (- a b))))

(defn trace-tail
  [head-path]
  (loop [[[head-x head-y :as head] & remainder] head-path
         tail-path                              [[0 0]]]
    (if-not head tail-path
      (let [[tail-x tail-y :as tail] (peek tail-path)
            mid-x                    (/ (+ head-x tail-x) 2)
            mid-y                    (/ (+ head-y tail-y) 2)]
        (recur
          remainder
          (conj tail-path
            (cond
              (adjacent? head tail)            tail
              (= head-x tail-x)                [tail-x mid-y]
              (= head-y tail-y)                [mid-x tail-y]
              (and (two-apart? head-x tail-x)
                   (two-apart? head-y tail-y)) [mid-x mid-y]
              (two-apart? head-x tail-x)       [mid-x head-y]
              (two-apart? head-y tail-y)       [head-x mid-y])))))))

(defn part-1 []
  (-> (reduce add-coords [[0 0]] motions)
      (trace-tail)
      (distinct)
      (count)))

(defn part-2 []
  (->> (reduce add-coords [[0 0]] motions)
       (iterate trace-tail)
       (take 10)
       (last)
       (distinct)
       (count)))
