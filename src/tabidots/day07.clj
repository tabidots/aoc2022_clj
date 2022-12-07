(ns tabidots.day07
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(comment
 "Day 7: No Space Left On Device")

(def terminal
  (-> "../resources/day07.txt" io/resource io/reader line-seq))

(defn input-map
  [raw]
  {:fs {} :commands raw :current [] :payload {}})

(defn drop-last-vec
  [coll]
  (subvec coll 0 (dec (count coll))))

(defn assemble-fs
  [{:keys [fs commands current payload]}]
  (if (nil? commands)
    (update-in fs current merge payload)
    (let [[this & those] commands
          [a b c]        (re-seq #"\S+" this)
          current'       (if (= b "cd")
                           (if (= c "..")
                             (drop-last-vec current)
                             (conj current c))
                           current)
          payload'      (cond
                          (= a "dir")        (assoc payload b {})
                          (re-find #"\d+" a) (assoc payload b (read-string a))
                          (= b "cd")         {}
                          :else              payload)
          fs'           (if (and (not-empty current) (= b "cd"))
                          (update-in fs current merge payload)
                          fs)]
      (recur
        {:fs       fs'
         :commands those
         :current  current'
         :payload  payload'}))))

(defn get-all-dirs
  [fs]
  (->> (tree-seq map? vals fs)
       (rest)
       (filter map?)))

(defn get-dir-size
  [dir]
  (->> (tree-seq map? vals dir)
       (filter number?)
       (apply +)))

(defn get-all-dir-sizes
  [input]
  (->> (input-map input)
       (assemble-fs)
       (get-all-dirs)
       (map get-dir-size)))

(defn part-1 []
  (->> (get-all-dir-sizes terminal)
       (filter #(< % 100000))
       (apply +)))

(defn part-2 []
  (let [[total-used & dir-sizes] (sort > (get-all-dir-sizes terminal))
        total-free               (- 70000000 total-used)
        need-to-clear            (- 30000000 total-free)]
    (->> dir-sizes
         (filter #(> % need-to-clear))
         (sort <)
         (first))))
