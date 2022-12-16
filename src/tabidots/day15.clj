(ns tabidots.day15
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 15: Beacon Exclusion Zone")

(def sample-input
  (-> "../resources/day15_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day15.txt" io/resource io/reader line-seq))

(defn manhattan
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn make-plot
  "Given text input with coordinates, outputs a vector [a b] where a is a list
  of maps describing each sensor's position and its range, and b is a hash-set of
  coordinates where the beacons are."
  [input]
  (->> (for [coords                (map (partial re-seq #"-?\d+") input)
             :let [[sensor beacon] (split-at 2 (map read-string coords))]]
         {:position        (vec sensor)
          :manhattan-range (manhattan sensor beacon)
          :beacon          (vec beacon)})
       ((juxt (partial map #(dissoc % :beacon))
              (comp set (partial map :beacon))))))

(defn range-at-row
  "Range of sensor at target row."
  [target-row {:keys [position manhattan-range]}]
  (let [[x y]      position
        dist-y     (Math/abs (- target-row y))
        max-dist-x (- manhattan-range dist-y)]
    (->> (range (- x max-dist-x) (inc (+ x max-dist-x)))
         (map #(vector % target-row))
         (seq))))

(defn part-1
  [input target-row]
  (let [[sensors beacons] (make-plot input)]
    (->> (mapcat (partial range-at-row target-row) sensors)
         (set)
         (remove beacons)
         (count))))

; (part-1 puzzle-input 2000000)

(defn tuning-frequency
  [[x y]]
  (+ (* 4000000 x) y))

(defn outside-perimeter
  [{:keys [position manhattan-range]}]
  (let [[x y]    position
        top-y    (- y (inc manhattan-range))
        bottom-y (+ y (inc manhattan-range))
        left-x   (- x (inc manhattan-range))
        right-x  (+ x (inc manhattan-range))]
    (concat (map vector (range x right-x)                 (range top-y y))
            (map vector (reverse (range x (inc right-x))) (range y bottom-y))
            (map vector (reverse (range left-x (inc x)))  (reverse (range (inc y) (inc bottom-y))))
            (map vector (range left-x x)                  (reverse (range (inc top-y) (inc y)))))))

(defn part-2
  [input bound]
  (let [[sensors _] (make-plot input)
        untouched?  (fn [[x y :as coords]]
                      (and (<= 0 x bound)
                           (<= 0 y bound)
                           (every? #(> (manhattan coords (:position %))
                                       (:manhattan-range %))
                                   sensors)))]
    (first
      (for [perimeter (map outside-perimeter sensors)
            :let [winner (some->> (filter untouched? perimeter)
                                  (seq)
                                  (first))]
            :when winner]
        (tuning-frequency winner)))))

; (part-2 puzzle-input 4000000)
; 6-minute runtime. Ouch
