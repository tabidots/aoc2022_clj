(ns tabidots.day23
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(comment
 "Day 23: Unstable Diffusion")

(def sample-input
  (-> "../resources/day23_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day23.txt" io/resource io/reader line-seq))

(defn parse
  [input]
  (apply merge
    (for [[y row] (map-indexed (fn [i x] [i (map str x)]) input)]
      {y (apply merge {}
           (for [[x cell] (map-indexed vector row)
                 :when (= cell "#")]
             {x {:position [y x] :proposal nil}}))})))

(defn rotate
  [[h & tail]]
  (vec (concat tail (list h))))

(def direction-sequence
  (zipmap (range 4)
          (take 4 (iterate rotate {:north [:nw :n :ne]
                                   :south [:sw :s :se]
                                   :west  [:nw :w :sw]
                                   :east  [:ne :e :se]}))))

(defn neighbors
  [[y x] board]
  (->> [[(dec y) (dec x)] [(dec y) x] [(dec y) (inc x)]
        [y (dec x)]                         [y (inc x)]
        [(inc y) (dec x)] [(inc y) x] [(inc y) (inc x)]]
       (map (partial get-in board))
       (zipmap [:nw :n :ne :w :e :sw :s :se])))

(defn propose
  [[y x :as position] board cur-round]
  (let [nbs (neighbors position board)]
    (if (not-any? some? (vals nbs)) nil ; no neighbors at all -> stay put
      (first ; otherwise, take the first direction with an empty batch of neighbors
        (for [[dir nb-batch] (direction-sequence (mod cur-round 4))
              :when (not-any? some? (vals (select-keys nbs nb-batch)))]
          (dir {:north [(dec y) x] ; and move in that direction
                :south [(inc y) x]
                :east  [y (inc x)]
                :west  [y (dec x)]}))))))

(defn keep-uniques
  [coll]
  (-> (group-by val (frequencies coll))
      (get 1)
      (keys)
      (set)))

(defn submit-proposals
  [board cur-round]
  (reduce-kv (fn [r y row]
               (assoc r y
                 (apply merge
                   (for [[x {:keys [position] :as elf}] row
                         :let [proposal (propose position board cur-round)]]
                     {x (assoc elf :proposal proposal)}))))
             {} board))

(defn hear-proposals
  [board]
  (->> (vals board)
       (remove empty?)
       (mapcat vals)
       (map :proposal)
       (remove nil?)
       (keep-uniques)))

(defn move-all-elves
  [board valid-proposals]
  (reduce-kv (fn [r y row]
               (apply merge-with merge r
                 (for [[x {:keys [proposal] :as elf}] row]
                   (if (some->> proposal (get valid-proposals))
                     (let [[py px] proposal]
                       {py {px {:position [py px] :proposal nil}}})
                     {y {x (assoc elf :proposal nil)}}))))
             {} board))

(defn one-round
  [{:keys [board cur-round] :as state}]
  (let [with-proposals  (submit-proposals board cur-round)
        valid-proposals (hear-proposals with-proposals)
        new-board       (move-all-elves with-proposals valid-proposals)]
    (when-not (= board new-board)
      (-> state
        (update :cur-round inc)
        (assoc :board new-board)))))

(defn sides
  [board]
  (reduce-kv (fn [{:keys [left right] :as r} y row]
               (let [xs (keys row)]
                 (-> r
                     (update :left  (partial apply min) xs)
                     (update :right (partial apply max) xs))))
             {:left 5000 :right 0} board))

(defn minimal-square
  [board]
  (let [{:keys [left right]} (sides board)
        rows (keys (remove (comp empty? val) board))]
    (* (inc (- (apply max rows) (apply min rows)))
       (inc (- right left)))))

(defn part-1
  [input]
  (let [head-count  (count (re-seq #"#" (apply str input)))
        final-state (->> {:cur-round 0 :board (parse input)}
                        (iterate one-round)
                        (rest)
                        (take 10)
                        (last)
                        :board)]
    (- (minimal-square final-state) head-count)))

(defn part-2
  [input]
  (->> {:cur-round 0 :board (parse input)}
       (iterate one-round)
       (take-while some?)
       (count)))
