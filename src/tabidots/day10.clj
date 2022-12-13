(ns tabidots.day09
  (:gen-class)
  (:require [clojure.java.io :as io]))
            
(comment
 "Day 10: Cathode-Ray Tube")

(def sample-1
  (s/split-lines
   "noop
addx 3
addx -5"))

(def sample-2
  (-> "../resources/day10_sample.txt" io/resource io/reader line-seq))

(def program
  (-> "../resources/day10.txt" io/resource io/reader line-seq))

(defn read-one-line
  [state line]
  (let [current     (peek state)
        instruction (re-find #"^[a-z]+" line)
        value       (some-> (re-find #"-?\d+$" line) read-string)]
    (case instruction
      "noop" (conj state current)
      "addx" (into state [current (+ current value)]))))

(defn part-1 []
  (let [signal    (reduce read-one-line [1] program)
        strengths (map * signal (iterate inc 1))]
    (->> (drop 19 strengths)
         (take-nth 40)
         (take 6)
         (apply +))))

(defn generate-message []
  (loop [[sprite-x & xs] (reduce read-one-line [1] program)
         crt             []
         cur-crt-row     []]
    (if-not xs (conj crt cur-crt-row)
      (let [cursor (if (= 40 (count cur-crt-row)) 0
                     (count cur-crt-row))
            pixel (if (#{(dec sprite-x) sprite-x (inc sprite-x)}  ; if sprite width contains
                        cursor)                                   ; cursor position
                    "#" ".")]
        (if (= 40 (count cur-crt-row))
          (recur xs (conj crt cur-crt-row) [pixel])
          (recur xs crt (conj cur-crt-row pixel)))))))

(defn part-2 []
  (doseq [line (generate-message)]
    (println line)))
