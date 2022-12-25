(ns tabidots.day25
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 25: Full of Hot Air")

(def sample-input
  (-> "../resources/day25_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day25.txt" io/resource io/reader line-seq))

(def conversions
  {\2 2 \1 1 \0 0 \- -1 \= -2
   2 2 1 1 0 0 4 "-" 3 "="})

(defn fifth [x] (long (Math/pow 5 x))) ; cast to long

(defn snafu->dec
  [s]
  (apply + (map (fn [a b]
                  (* (conversions a) (fifth b)))
                (seq s)
                (reverse (range (count s))))))

(defn dec->snafu
 [x]
 (loop [n     x
        snafu ""
        place 1]
   (let [q  (quot n place)
         q' (mod q 5)]
     (if (zero? q) snafu
       (recur
         (case q'
           3 (+ n (* 2 place))
           4 (+ n place)
           n)
         (str (conversions q') snafu)
         (* place 5))))))
