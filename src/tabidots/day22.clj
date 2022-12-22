(ns tabidots.day22
  (:gen-class)
  (:require [clojure.java.io :as io]))

(comment
 "Day 22: Monkey Map")

(def sample-input
  (-> "../resources/day22_sample.txt" io/resource io/reader line-seq))

(def puzzle-input
  (-> "../resources/day22.txt" io/resource io/reader line-seq))

(defn pad-string
  [line target]
  (apply str line (repeat (- target (count line)) " ")))

(defn parse-board
  [board]
  (apply merge
    (for [[idx items] (map-indexed (fn [i x] [i (map str x)]) board)
          :let [mnm   (->> [(.indexOf items ".") (.indexOf items "#")]
                           (remove neg?) ; nonexistent returns -1
                           (apply min))
                mxm   (max (.lastIndexOf items ".") (.lastIndexOf items "#"))
                walls (set (keep-indexed (fn [i x]
                                           (when (= x "#") i))
                                         items))]]
      {idx {:mnm mnm :mxm mxm :walls walls}})))

(defn init-state
  [input]
  (let [[board' _ [path]] (partition-by empty? input)
        board  (map #(pad-string % (apply max (map count board')))
                    board') ; pad all rows to match length of longest line
        rows   (parse-board board)]
    {:location {:facing 0 :y 0 :x (get-in rows [0 :mnm])}
     :path     (map #(if (re-find #"\d+" %) (read-string %) %)
                    (re-seq #"\d+|[LR]" path))
     :rows     rows
     :cols     (parse-board (apply mapv vector board))}))

(defn mod-4 [x] (mod x 4))

(defn proceed
  [{:keys [location rows cols] :as state} instruction]
  (let [{:keys [facing y x]} location]
    (case instruction
      "R" (update-in state [:location :facing] (comp mod-4 inc))
      "L" (update-in state [:location :facing] (comp mod-4 dec))
      ;; Otherwise, instruction is a number -> number of steps
      (let [{:keys [mnm mxm walls] :as items}
            ,        (if (even? facing) (get rows y)
                       (get cols x))
            target   (if (even? facing) :x :y)]
        (loop [pos'  (if (even? facing) x y)
               steps instruction]
          (if (zero? steps) (assoc-in state [:location target] pos')
            (let [new-pos     ((case facing 0 inc 1 inc 2 dec 3 dec) pos')
                  wrapped-pos (cond
                                (> new-pos mxm) mnm
                                (< new-pos mnm) mxm
                                :else new-pos)]
              (if (walls wrapped-pos)
                (assoc-in state [:location target] pos')
                (recur wrapped-pos (dec steps))))))))))

(defn part-1 []
  (let [{:keys [path] :as state} (init-state puzzle-input)
        {:keys [y x facing]}     (:location (reduce proceed state path))]
    (+ (* 1000 (inc y)) (* 4 (inc x)) facing)))
