(ns tabidots.day20
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 20: Grove Positioning System")

(def sample-input
  [1 2 -3 3 -2 0 4])

(def puzzle-input
  (let [r (-> "../resources/day20.txt" io/resource io/reader line-seq)]
    (mapv read-string r)))

(defn unmixed
  [input & {:keys [:part-2?] :or {part-2? false}}]
  (map-indexed (fn [i x]
                 (hash-map :order i :position i
                           :value (if part-2?
                                    (* 811589153 x)
                                    x)))
               input))

(defn mix
  [file']
  (let [m        (dec (count file'))
        loop-mod (fn [a b] ; Prevents two values from having index 0
                   (let [r (mod (+ a b) m)]
                     (if (and (zero? r) (pos? b)) m r)))]
    (loop [k 0 file file']
      (if (> k m) file
        (let [target              (first (filter #(= k (:order %)) file))
              {old-pos :position,
               value   :value}    target
              new-pos            (loop-mod old-pos value)]
          (recur
            (inc k)
            (->> file
                 (mapv (fn [{:keys [position] :as item}]
                         (cond
                           (= item target)
                           , (assoc target :position new-pos)
                           (and (< old-pos new-pos)  ; target ends up right of where it started
                                (<= old-pos position new-pos))
                           , (update item :position loop-mod -1)
                           (and (< new-pos old-pos)  ; target ends up left of where it started
                                (<= new-pos position old-pos))
                           , (update item :position loop-mod 1)
                           :else
                           , item))))))))))

(defn grove-coords-sum
  [v]
  (reduce (fn [res target-idx]
            (+ res
              (nth v (mod (+ (.indexOf v 0) target-idx)
                          (count v)))))
          0 [1000 2000 3000]))

(defn part-1 []
  (->> (unmixed puzzle-input)
       (mix)
       (sort-by :position)
       (mapv :value)
       (grove-coords-sum)))

(defn part-2 []
  (->> (unmixed puzzle-input :part-2? true)
       (iterate mix)
       (rest)
       (take 10)
       (last)
       (sort-by :position)
       (mapv :value)
       (grove-coords-sum)))
