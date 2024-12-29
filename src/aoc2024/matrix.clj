(ns aoc2024.matrix)

(defn det
  "Returns the determinant of the given matrix."
  [[[a b][c d]]]
  (- (* a d) (* b c)))

(defn invert
  "Returns the inversion of the given matrix."
  [[[a b][c d] :as m]]
  (let [dv (det m)]
    [[(/ d dv) (- (/ b dv))]
     [(- (/ c dv)) (/ a dv)]]))

(defn mul
  "Returns the product of a matrix and a vector."
  [[[a b][c d]] [x y]]
  [(+ (* a x) (* b y))
   (+ (* c x) (* d y))])

(defn solve-sole
  "Solve a system of linear equations, A * x = B.
  With A being a 2d matrix [[a11 a12][a21 a22]] (row or row-vecs)
  and B begin a 1d vector [b1 b2].
  Returns the vector [x1 x2]."
  [A B]
  (-> A
      (invert)
      (mul B)))

(comment
  (det [[1 2] [3 4]])
  (invert [[1 2] [3 4]])
  (mul [[1 2] [3 4]] [10 10])
)
