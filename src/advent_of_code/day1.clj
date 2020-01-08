(ns advent-of-code.day1
  (:require clojure.string))

(defn fuel-requirement [m]
  (let [result (- (quot m 3) 2)]
    (if (<= result 0)
      0
      (+ result (fuel-requirement result)))))

(defn sum-fuel-requirement
  [ms]
    (->> ms
         (map fuel-requirement)
         (reduce +)))

(= 34241 (sum-fuel-requirement [12 14 1969 100756]))

(def input (map #(Integer/parseInt %)
                (clojure.string/split (slurp "data/1") #"\n")))

(sum-fuel-requirement input)
