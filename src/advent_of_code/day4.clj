(ns advent-of-code.day4)

(def MIN 382345)
(def MAX 843167)

(defn is-6-digits [n]
  (= 6 (count (str n))))

(is-6-digits 1000)
(is-6-digits MIN)

(defn has-2-adjacent-digits [n]
  (> (count (filter (fn [[a b]] (= a b))
                    (map vector (str n) (rest (str n)))))
     0))

(has-2-adjacent-digits 123)
(has-2-adjacent-digits 1233)

(defn never-decreases-left-to-right [n]
  (apply <= (map #(Integer/parseInt (str %)) (str n))))

(never-decreases-left-to-right 123)
(never-decreases-left-to-right 111)
(never-decreases-left-to-right 121)

(defn meets-criteria [n]
  (and (is-6-digits n)
       (has-2-adjacent-digits n)
       (never-decreases-left-to-right n)))

(meets-criteria MIN)
(meets-criteria MAX)

(count (filter meets-criteria (range MIN MAX)))

(defn has-2-and-no-more-same-digits [n]
  (> (count (filter #(= 2 %) (vals (frequencies (str n)))))
   0))

(has-2-and-no-more-same-digits 112233)
(has-2-and-no-more-same-digits 123444)
(has-2-and-no-more-same-digits 111122)
(has-2-and-no-more-same-digits 123455)

(defn meets-part-2-criteria [n]
  (and (is-6-digits n)
       (never-decreases-left-to-right n)
       (has-2-and-no-more-same-digits n)))

(meets-part-2-criteria 112233)
(meets-part-2-criteria 123444)
(meets-part-2-criteria 111122)
(meets-part-2-criteria 111122)
(meets-part-2-criteria 123455)

(count (filter meets-part-2-criteria (range MIN MAX)))
