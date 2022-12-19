(ns tabidots.day18
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.set :refer [difference]]))

(comment
 "Day 18: Boiling Boulders")

(def sample-input
  (-> "../resources/day18_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day18.txt" io/resource io/reader line-seq))

(defn parse
  [input]
  (set
    (for [line input]
      (map read-string (re-seq #"\d+" line)))))

(defn neighbors
  [[x y z]]
  [[(dec x) y z] [(inc x) y z]
   [x (dec y) z] [x (inc y) z]
   [x y (dec z)] [x y (inc z)]])

(defn surface-area
  [cubes]
  (reduce (fn [res cube]
            (+ res (- 6 (count (keep cubes (neighbors cube))))))
          0 cubes))

(defn part-1 [input]
  (surface-area (parse input)))

(defn find-pockets
  "Given a set of cube coordinates, a pair of boundary coordinates, and target coordinates
  of an empty cell, categorizes the cell as being part of an internal air pocket or
  simply on the outside of the droplet. `Cell` must be an empty cell."
  ; Why are we keeping the outside cells? To speed up the `reduce` later as we scan
  ; ALL empty cells.
  [cubes min-edge max-edge cell]
  (loop [stack  #{cell}
         closed #{}]
    (cond
      (some #{min-edge max-edge} (apply concat stack))
      ,               {:outside closed}
      (empty? stack)  {:pockets closed}
      :else           (recur
                        (->> stack
                             (keep (fn [cube]
                                     (when-not (closed cube)
                                       (remove cubes (neighbors cube)))))
                             (apply concat)
                             (set))
                        (into closed stack)))))

(defn part-2
  [input]
  (let [cubes               (parse input)
        [min-edge max-edge] ((juxt (partial apply min) ; Assume coordinate space is a perfect cube
                                   (partial apply max)) (apply concat cubes))
        all-empty-cells     (for [x (range min-edge (inc max-edge))
                                  y (range min-edge (inc max-edge))
                                  z (range min-edge (inc max-edge))
                                  :when (not (cubes [x y z]))]
                              [x y z])
        pockets             (->> all-empty-cells
                                 (reduce (fn [{:keys [outside pockets] :as res} cell]
                                           (if (pockets cell) res
                                             (if-let [p (find-pockets cubes min-edge max-edge cell)]
                                               (merge-with into res p)
                                               res)))
                                         {:outside #{} :pockets #{}})
                                 :pockets)]
    (- (surface-area cubes) (surface-area pockets))))
