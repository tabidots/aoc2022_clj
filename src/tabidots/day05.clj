(ns tabidots.day05
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(comment
 "Day 5: Supply Stacks")

(def sample-input
  (-> "../resources/day05_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day05.txt" io/resource io/reader line-seq))

(defn read-crates
  "Extracts letters from the crate lines or spaces if blank."
  [line]
  (->> (partition-all 4 4 line)
       (map (comp str second)))) ; coerce from char -> string

(defn stack-crates
  "Turns the vertical representation of crate stacks into a map of lists where
  the list at index n represents stack n, with crates ordered from top to bottom.
  e.g., {2 (A B C)} = A on top, B in middle, C on bottom of stack 2."
  [raw-crates]
  (->> (map read-crates raw-crates)
       (drop-last)                     ; Drop the indexes
       (apply map list)                ; Vertical -> horizontal. Use lists so that conj prepends
       (map (partial remove s/blank?)) ; No blanks
       (zipmap (iterate inc 1))))

(defn parse
  "Parses the input into crates and instructions. CrateMover model number must
  be 9000 or 9001."
  [input model-num]
  (let [[crates _ instructions] (partition-by empty? input)]
    {:crates (stack-crates crates)
     :instructions instructions
     :model-num model-num}))

(defn move-crates
  "Executes one step of the instructions. Only CrateMover 9000 reverses the moved crates."
  [crates how-many origin destination model-num]
  (let [payload (cond->>                (get crates origin)
                     :always            (take how-many)
                     (= model-num 9000) (reverse))] ; first-out, last-in
    (-> crates
        (update destination conj payload)
        (update destination flatten)
        (update origin (partial drop how-many)))))

(defn read-top-crates
  [crates]
  (apply str (map first (vals (sort crates)))))

(defn run-procedure
  [{:keys [crates instructions model-num]}]
  (if (nil? instructions)
    (read-top-crates crates)
    (let [[how-many origin destination] (->> (first instructions)
                                             (re-seq #"\d+")
                                             (map read-string))]
      (recur
        {:crates       (move-crates crates how-many origin destination model-num)
         :instructions (next instructions)
         :model-num    model-num}))))

(defn part-1 []
  (run-procedure (parse puzzle-input 9000)))

(defn part-2 []
  (run-procedure (parse puzzle-input 9001)))
