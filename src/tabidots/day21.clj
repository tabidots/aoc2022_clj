(ns tabidots.day21
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 21: Monkey Math")

(def sample-input
  (-> "../resources/day21_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day21.txt" io/resource io/reader line-seq))

(defn monkey-jobs
  [input]
  (apply merge
    (for [line input
          :let [[id a op b] (->> (re-seq #"[^\s:]+" line)
                                 (map (fn [x]
                                        (if (some->> x (re-find #"[\d+-\/\*]+"))
                                          (read-string x)
                                          x))))]]
      (if b
        {id {:op (symbol op) :a a :b b}}
        {id {:number a}}))))

(defn calculate
  [x jobs]
  (if (number? x) x ; cases where either or both operands are numbers
    (let [{:keys [op a b number]} (get jobs x)]
      (if number number ; if there is a key :number, that's the answer
        ((resolve op) (calculate a jobs) (calculate b jobs))))))

(defn part-1
  []
  (calculate "root" (monkey-jobs sample-input)))

(defn solve-backward
  [chain]
  (reduce (fn [res {:keys [op a b] :as job}]
            (let [x (if (number? a) a b)]
              (case op
                = x
                + (- res x)
                - (if (number? a)  ; tricky case!
                    (- a res)
                    (+ res b))
                * (/ res x)
                / (* res x))))
          0 chain))

(defn trace-human
  [input]
  (let [jobs (assoc-in (monkey-jobs input) ["root" :op] '=)]
    (loop [current "humn"
           chain   '()]
      (if (= current "root") (solve-backward chain)
        (let [[next-monkey {:keys [a b] :as next-job}]
              ,       (first (filter (fn [[k {:keys [a b]}]]
                                       (or (= a current) (= b current)))
                                     jobs))
              next-op (cond-> next-job
                        (not= a current)         (update :a calculate jobs)
                        (and b (not= b current)) (update :b calculate jobs))]
          (recur
            next-monkey
            (cons next-op chain)))))))

(defn part-2 []
  (trace-human puzzle-input))
