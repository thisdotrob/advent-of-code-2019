(ns advent-of-code.core)

;; Day 1

(defn fuel-requirement [m]
  (let [result (- (quot m 3) 2)]
    (println result)
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


;; Day 2

(defn run-intcode-program
  ([p] (run-intcode-program 0 p))
  ([n p] (let [operator (nth p n)]
           (if (= operator 99)
             p
             (let [operand-a (nth p (nth p (+ n 1)))
                   operand-b (nth p (nth p (+ n 2)))
                   new-pos (nth p (+ n 3))
                   new-val (if (= operator 1)
                             (+ operand-a operand-b)
                             (* operand-a operand-b))]
               (run-intcode-program (+ n 4) (assoc p new-pos new-val)))))))

(run-intcode-program [99])
(run-intcode-program [1 9 10 3 2 3 11 0 99 30 40 50])
(run-intcode-program [1 0 0 0 99])
(run-intcode-program [2 3 0 3 99])
(run-intcode-program [2 4 4 5 99 0])
(run-intcode-program [1 1 1 4 99 5 6 0 99])

(def puzzle-input [1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 6 1 19 1 5 19 23 2 9 23 27 1 6 27 31 1 31 9 35 2 35 10 39 1 5 39 43 2 43 9 47 1 5 47 51 1 51 5 55 1 55 9 59 2 59 13 63 1 63 9 67 1 9 67 71 2 71 10 75 1 75 6 79 2 10 79 83 1 5 83 87 2 87 10 91 1 91 5 95 1 6 95 99 2 99 13 103 1 103 6 107 1 107 5 111 2 6 111 115 1 115 13 119 1 119 2 123 1 5 123 0 99 2 0 14 0])

(-> puzzle-input
    (assoc 1 12)
    (assoc 2 2)
    run-intcode-program
    first)

(def desired-output 19690720)

(def nums (range 0 100))

(def inputs (reduce (fn [ps n1] (concat ps (map (fn [n2] [n1 n2]) nums))) [] nums))

(doseq [[noun verb] inputs]
  (let [result (-> puzzle-input
                   (assoc 1 noun)
                   (assoc 2 verb)
                   run-intcode-program
                   first)]
    (if (= result 19690720)
      (println noun verb))))

(+ (* 100 84) 78)


;; Day 3

(defn instruction->coord [prev-coord instruction]
  (let [direction (first instruction)
        distance (Integer/parseInt (subs instruction 1))
        distance-range (range 1 (+ distance 1))
        x (first prev-coord)
        y (second prev-coord)]
    (cond
      (= direction \R) (map (fn [d] [(+ x d) y]) distance-range)
      (= direction \L) (map (fn [d] [(- x d) y]) distance-range)
      (= direction \U) (map (fn [d] [x (+ y d)]) distance-range)
      (= direction \D) (map (fn [d] [x (- y d)]) distance-range))))

(defn instructions->coords [instructions]
  (reduce (fn [coords instruction]
            (let [prev-coord (or (last coords) [0,0])]
              (concat coords (instruction->coord prev-coord instruction))))
          []
          instructions))

(defn intersections [input]
  (let [instructions (map #(clojure.string/split % #",")
                          (clojure.string/split input #"\n"))
        coords (map instructions->coords instructions)]
    (clojure.set/intersection (set (first coords)) (set (second coords)))))

(defn manhattan [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn shortest-manhattan [input]
  (first (sort (map manhattan (intersections input)))))

(def example-input-1 "R8,U5,L5,D3\nU7,R6,D4,L4")
(def example-input-2 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")
(def example-input-3 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
(def actual-input (slurp "data/3"))

(shortest-manhattan example-input-1)
(shortest-manhattan example-input-2)
(shortest-manhattan example-input-3)
(shortest-manhattan actual-input)
