(ns tabidots.day13
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(comment
 "Day 13: Distress Signal")

(def sample-input
  (-> "../resources/day13_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day13.txt" io/resource io/reader line-seq))

;; Some test pairs from the Reddit thread for debugging
(def true-pair [[[3 [7] [0] 7]]
                [[5 2]]])
(def false-pair [[[8 [[7]]]]
                 [[[[[8]]]]]])

(defn pair-packets
  [input]
  (->> (partition-by s/blank? input)
       (remove #(= (count %) 1))
       (map #(map read-string %))))

(defn map-longest ; https://stackoverflow.com/a/18940745
  [f default & colls]
  (lazy-seq
   (when (some seq colls)
     (cons
      (apply f (map #(if (seq %) (first %) default) colls))
      (apply map-longest f default (map rest colls))))))

(defn compare-packets
  [[left right :as pair]]
  (cond
    (nil? right)           false               ; right pair runs out first
    (nil? left)            true                ; left pair runs out first
    (every? integer? pair) (if (= left right)
                             :equal            ; use equal to defer truth value
                             (< left right))
    (integer? right)       (compare-packets [left (vector right)])
    (integer? left)        (compare-packets [(vector left) right])
    :else
    (if-some [[first-unequal-subpair & _] ; first pair decides(!), so ignore the rest
              ;; ↓ pad the pairs with nils to determine which is longer, if necessary
              (some->> (apply map-longest vector nil pair)
                       (remove #(= :equal (compare-packets %)))
                       (seq))]
      (compare-packets first-unequal-subpair)
      :equal))) ; no unequal subpairs? -> don't return true/false yet

(defn part-1
  []
  (apply + (keep-indexed (fn [i x]
                           (when (compare-packets x)
                             (inc i)))
                         (pair-packets puzzle-input))))

(defn packets-with-dividers
  [input]
  (conj (mapv read-string (remove s/blank? input))
        [[2]] [[6]]))

(defn basic-sort ; extremely basic exchange sort, highly inefficient
  [input]
  (loop [index   0
         packets input]
    (cond
      (every? compare-packets (partition 2 1 packets)) packets
      (= (inc index) (count packets))                  (recur 0 packets)
      :else
      (let [less (subvec packets 0 index)
            pair (subvec packets index (+ 2 index))
            more (subvec packets (+ 2 index))]
        (recur (inc index)
          (if (compare-packets pair) ; correct order
            packets
            (vec (concat less (reverse pair) more))))))))

(defn part-2 []
  (->> (packets-with-dividers puzzle-input)
       (basic-sort)
       (keep-indexed (fn [i x]
                       (when (#{[[2]] [[6]]} x)
                         (inc i))))
       (apply *)))
