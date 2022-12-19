(ns tabidots.day16
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.set :refer [difference]]
            [clojure.math.combinatorics :refer [combinations]]))

(comment
 "Day 16: Proboscidea Volcanium")

(def sample-input
  (-> "../resources/day16_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day16.txt" io/resource io/reader line-seq))

(defn parse
  [input]
  (apply merge
    (for [line input
          :let [[_                   id     raw-flow-rate  raw-neighbors]
                (re-find #"Valve ([A-Z]{2}).+=(\d+).+valves? (.+)" line)
                flow-rate  (read-string raw-flow-rate)
                neighbors  (re-seq #"[A-Z]{2}" raw-neighbors)]]
      {id {:id        id
           :flow-rate flow-rate
           :neighbors neighbors}})))

(defn assoc-in!
  "assoc-in for transients. Only supports one level of nesting."
  [m [k kk] v]
  (assoc! m k (merge (get m k) {kk v})))

(defn distance-matrix
  ; https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm
  [parsed-input]
  (let [valves    (keys parsed-input)
        matrix    (transient
                    (apply merge
                      (for [[self {:keys [neighbors]}] parsed-input
                            :let [neighbor-map (apply merge (map #(hash-map % 1) neighbors))]]
                        {self (-> (zipmap valves (repeat 10000))
                                  (merge neighbor-map)
                                  (merge {self 0}))})))]
    (doseq [k valves
            i valves
            j valves
            :when (> (get-in matrix [i j])
                     (+ (get-in matrix [i k]) (get-in matrix [k j])))]
      (assoc-in! matrix [i j] (+ (get-in matrix [i k]) (get-in matrix [k j]))))
    (persistent! matrix)))

(defn init-state
  [input]
  (let [valves (parse input)]
    {:minute             0
     :remaining-distance 0
     :at-valve           "AA"
     :open-valves        #{}
     :pressure           0
     :valves             valves
     :distances          (distance-matrix valves)}))

(defn release-pressure
  [time-limit {:keys [minute remaining-distance at-valve open-valves pressure distances valves history] :as state}]
  (let [all-pressures (apply + (map :flow-rate open-valves))]
    (cond (> minute time-limit) pressure ; terminate
      (pos? remaining-distance) ; Moving to a valve
      , (release-pressure time-limit
          (-> state
              (update :remaining-distance dec)
              (update :minute             inc)
              (update :pressure           + all-pressures)))
      :else ; Turning on a valve and choosing the next one
      , (apply max
          (for [[next-valve {:keys [flow-rate]}] valves
                :when (and (not= next-valve at-valve)
                           (not (get open-valves next-valve))
                           (pos? flow-rate))]
            (release-pressure time-limit
              (-> state
                  (assoc  :at-valve           next-valve)
                  (assoc  :remaining-distance (get-in distances [at-valve next-valve]))
                  (update :open-valves        conj (valves at-valve))
                  (update :minute             inc)
                  (update :pressure           + all-pressures))))))))

(defn part-1 []
  (release-pressure 30 (init-state puzzle-input))) ; runtime 90s

(defn part-2 [] ; runtime about a half hour
  (let [global-init   (init-state puzzle-input)
        useful-valves (->> (:valves global-init)
                           (keep (fn [[id {:keys [flow-rate]}]]
                                   (when (pos? flow-rate) id))))
        half          (int (Math/ceil (/ (count useful-valves) 2)))]
    (apply max
      (for [mv (combinations useful-valves half)
            :let [ev       (difference (set useful-valves) (set mv))
                  me       (update global-init :valves select-keys (conj mv "AA"))
                  elephant (update global-init :valves select-keys (conj ev "AA"))]]
        (+ (release-pressure 26 me)
           (release-pressure 26 elephant))))))
