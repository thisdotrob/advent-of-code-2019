(ns advent-of-code.day3
  (:require clojure.set))

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

(defn instruction->line [prev-point instruction]
  (let [direction (first instruction)
        distance (Integer/parseInt (subs instruction 1))
        distance-range (range 1 (+ distance 1))
        x (:x prev-point)
        y (:y prev-point)
        start-steps (:steps prev-point)]
    (cond
      (= direction \R)
      (map (fn [d] {:steps (+ start-steps d) :x (+ x d) :y y}) distance-range)

      (= direction \L)
      (map (fn [d] {:steps (+ start-steps d) :x (- x d) :y y}) distance-range)

      (= direction \U)
      (map (fn [d] {:steps (+ start-steps d) :x x :y (+ y d)}) distance-range)

      (= direction \D)
      (map (fn [d] {:steps (+ start-steps d) :x x :y (- y d)}) distance-range))))

(defn instructions->line [instructions]
  (reduce (fn [line instruction]
            (let [prev-point (or (last line) {:steps 0 :x 0 :y 0})]
              (concat line (instruction->line prev-point instruction))))
          []
          instructions))

(defn shortest-steps [line-a line-b]
  (loop [shortest-steps nil
         line line-a]
    (if line
      (let [point (first line)
            steps (:steps point)]
        (if (or (not shortest-steps) (< steps shortest-steps))
          (let [intersections (filter #(and (= (:x %) (:x point))
                                            (= (:y %) (:y point)))
                                      line-b)
                shortest (:steps (first (sort-by :steps intersections)))]
            (recur (cond
                     (not shortest) shortest-steps
                     (not shortest-steps) (+ shortest steps)
                     (< (+ shortest steps) shortest-steps) (+ shortest steps)
                     :else shortest-steps)
                   (next line)))
          shortest-steps))
      shortest-steps)))

(def instructions (map #(clojure.string/split % #",")
                       (clojure.string/split actual-input #"\n")))

(def lines (map instructions->line instructions))
(def line-a (first lines))
(def line-b (second lines))
