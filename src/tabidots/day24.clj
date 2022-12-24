(ns tabidots.day24
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map-keyfn]]
            [clojure.pprint :refer [pprint]]))

(comment
 "Day 24: Blizzard Basin")

(def sample-input
  (-> "../resources/day24_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day24.txt" io/resource io/reader line-seq))

(defn parse
  [input]
  (apply merge-with conj {:upward #{} :rightward #{} :downward #{} :leftward #{}}
    (apply concat
      ;; decrement y and x so that the coordinates describe the valley without walls
      (for [[y row] (map-indexed (fn [i n] [(dec i) n]) input)]
        (for [[x cell] (map-indexed (fn [i n] [(dec i) n]) row)
              :when (not (#{\. \#} cell))]
          (case cell
            \> {:rightward [y x]}
            \< {:leftward [y x]}
            \^ {:upward [y x]}
            \v {:downward [y x]}))))))

(defn init-state
  [input]
  (let [height (- (count input) 2)
        width  (- (count (first input)) 2)]
    {:height    height
     :width     width
     :start     [-1 0] ; consider the start & goal as being outside the valley grid
     :goal      [height (dec width)]
     :blizzards (parse input)}))

(defn blizzard-positions
  "Given a map describing the original blizzard positions, the valley dimensions,
  and a target minute, returns the positions of all blizzards at the specified minute."
  [{:keys [blizzards height width]} minute]
  (reduce-kv (fn [r dir b-set]
               (into r
                 (map (fn [[y x]]
                        [(-> (case dir :upward (- y minute) :downward (+ y minute) y)
                             (mod height))
                         (-> (case dir :leftward (- x minute) :rightward (+ x minute) x)
                             (mod width))])
                      b-set)))
             #{} blizzards))

(defn manhattan-distance
  [[by bx] [ay ax]]
  (+ (Math/abs (- by ay)) (Math/abs (- ax bx))))

(defn a*
  [{:keys [blizzards height width goal start start-cost] :as input}]
  (let [heuristic-fn (partial manhattan-distance goal)
        neighbors-fn (fn [[y1 x1]]
                       (for [[y2 x2] [[1 0] [0 1] [0 0] [-1 0] [0 -1]]
                             :let [y' (+ y1 y2) x' (+ x1 x2)]
                             :when (or (#{start goal} [y' x'])
                                       (and (<= 0 y' (dec height))
                                            (<= 0 x' (dec width))))]
                         [y' x']))
        a*-cost      (fn [{:keys [cost heuristic]}] (+ cost heuristic))
        seed-info    {:cost (or start-cost 0)
                      :heuristic (heuristic-fn start)}]
    (loop [stack      (priority-map-keyfn a*-cost start seed-info)
           closed-set {}]
      (let [[coords {:keys [cost]} :as current] (peek stack)]
        (cond
          (nil? current)  nil ; No path, thanks for playing
          (= goal coords) (dec (- cost (or start-cost 0)))
          :else
          (let [neighbors
                (into {}
                  (for [neighbor (neighbors-fn coords)
                        :when (not ((blizzard-positions input cost) neighbor))
                        :let [n {neighbor {:cost (inc cost)
                                           :heuristic (heuristic-fn neighbor)}}]
                        :when (not= (closed-set neighbor) n)]
                     n))]
            (recur (merge-with (partial min-key a*-cost) (pop stack) neighbors)
              (conj closed-set current))))))))

(defn swap-ends
  [{:keys [goal start] :as state}]
  (-> state
    (assoc :goal start)
    (assoc :start goal)))

(defn part-1 [input] ; runtime 10 min
  (a* (init-state input)))

(defn part-2 [input] ; runtime 33 min
  (let [there       (a* (init-state input))
        back        (a* (-> (init-state input)
                            (swap-ends)
                            (assoc :start-cost there)))
        there-again (a* (-> (init-state input)
                            (assoc :start-cost (+ there back))))]
    (+ there back there-again)))
