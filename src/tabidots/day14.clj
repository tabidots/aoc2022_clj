(ns tabidots.day14
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 14: Regolith Reservoir")

(def puzzle-input
  (-> "../resources/day14.txt" io/resource io/reader line-seq))

(defn extract-corners
  [line]
  (map vec (partition 2 (map read-string (re-seq #"\d+" line)))))

(defn connect-corners
  [[x1 y1] [x2 y2]]
  (cond
    (and (= x1 x2) (< y1 y2)) (map #(vector x1 %) (range (inc y1) (inc y2))) ; rightward
    (and (= x1 x2) (> y1 y2)) (reverse (map #(vector x1 %) (range y2 y1)))   ; leftward
    (and (< x1 x2) (= y1 y2)) (map #(vector % y1) (range (inc x1) (inc x2))) ; downward
    (and (> x1 x2) (= y1 y2)) (reverse (map #(vector % y1) (range x2 x1))))) ; upward

(defn draw-line
  [line]
  (reduce (fn [res b]
            (if-some [a (peek res)]
              (into res (connect-corners a b))
              (conj res b)))
          [] (extract-corners line)))

(defn init-state
  "Sets the initial state of the cave based on a list of coordinates.
  Mode must be :abyss (Part 1) or :floor (Part 2)."
  ;; We can use the same bottom value for both parts. In Part 1, if y reaches
  ;; the maximum y-value + 1, we know that it will never stop. In Part 2,
  ;; if y reaches the maximum y-value + 1, it MUST stop.
  [input & {:keys [mode]}]
  (let [walls (mapcat draw-line input)]
    {:walls  (set walls)
     :bottom (inc (apply max (map peek walls)))
     :mode   mode}))

(defn produce-sand
  "Produces one grain of sand and traces its path until it can fall no further."
  [{:keys [walls bottom mode] :as state}]
  (loop [[x y :as position] [500 0]]
    (if (= y bottom) (case mode
                       :abyss nil ; ← terminate as soon as first grain falls into the abyss
                       :floor (update state :walls conj position)) ; ← can't fall further
      (let [down       [x (inc y)]
            down-left  [(dec x) (inc y)]
            down-right [(inc x) (inc y)]
            new-pos    (->> [down down-left down-right ; ← don't consider occupied spaces
                             (remove (partial get walls))
                             (first)])] ; ← order of priority is D, DL, DR
        (if new-pos
          (recur new-pos)
          (update state :walls conj position))))))

(defn part-1 []
  (->> (init-state puzzle-input :mode :abyss)
       (iterate produce-sand)
       (rest)
       (take-while some?)
       (count)))

(defn part-2 []
  (->> (init-state puzzle-input :mode :floor)
       (iterate produce-sand)
       (take-while #(not ((:walls %) [500 0]))) ; -> "while 500,0 not in :walls"
       (count)))
