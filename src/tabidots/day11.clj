(ns tabidots.day11
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(comment
 "Day 11: Monkey in the Middle")

(def sample-input
  (-> "../resources/day11_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day11.txt" io/resource io/reader line-seq))

(defn monkeys
  [input]
  (apply merge
    (for [input (partition-by s/blank? input)
          :when (> (count input) 1)
          :let  [[raw-id items raw-op raw-divisor
                  raw-test-true raw-test-false]    input
                 [_ op value]                      (re-find #"new = old (.) (.+$)" raw-op)
                 [id divisor test-true test-false] (->> [raw-id raw-divisor raw-test-true raw-test-false]
                                                        (map #(read-string (re-find #"\d+" %))))]]
      {id {:items           (mapv read-string (re-seq #"\d+" items))
           :items-inspected 0
           :worry-op        #((resolve (read-string op))
                              %
                              (if (= value "old") % (read-string value)))
           :divisor         divisor
           :div-test        #(zero? (mod % divisor))
           :test-true       test-true
           :test-false      test-false}})))

(defn relief
  [worry-score]
  (Math/floor (/ worry-score 3)))

(defn super-modulo
  [state]
  (let [super-divisor (apply * (map :divisor (vals state)))]
    #(mod % super-divisor)))

(defn one-round
  [state' & {:keys [part-2?] :or {part-2? false}}]
  (loop [whose-turn 0 state state']
    (if-some [{:keys [items worry-op mod-worry-op divisor div-test test-true test-false]} (get state whose-turn)]
      (let [worry-scores          (if part-2?
                                    (mapv (comp (super-modulo state) worry-op) items)
                                    (mapv (comp relief worry-op) items))
            [pass-test fail-test] ((juxt filter remove) div-test worry-scores)]
        (recur
          (inc whose-turn)
          (-> state
              (assoc-in  [whose-turn :items] [])
              (update-in [whose-turn :items-inspected] + (count items))
              (update-in [test-true  :items] into pass-test)
              (update-in [test-false :items] into fail-test))))
      state)))

(defn calculate-monkey-business
  [state rounds & {:keys [part-2?] :or {part-2? false}}]
  (->> state
       (iterate #(one-round % :part-2? part-2?))
       (take (inc rounds))
       (last)
       (vals)
       (map :items-inspected)
       (sort >)
       (take 2)
       (apply *)))

(part-1 []
  (calculate-monkey-business (monkeys puzzle-input) 20))

(part-2 []
  (calculate-monkey-business (monkeys puzzle-input) 10000 :part-2? true))
