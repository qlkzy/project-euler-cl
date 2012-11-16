;; Solution to Project Euler Problem 6

;; Finding the difference between the sum of the squares and the
;; square of the sums of the first 100 natural numbers.

(defun square-of-sums (l)
  "Return the square of the sums of a list"
  (expt (apply #'+ l) 2))

(defun sum-of-squares (l)
  "Return the sum of the squares of a list"
  (apply #'+
         (mapcar (lambda (x) (expt x 2))
                 l)))
(defun problem-6 (n)
  "Return the difference between the square of the sums and the sum of
  the squares of the first N natural numbers."
  (let ((numbers (loop for i from 1 to n collect i)))
    (- (square-of-sums numbers)
       (sum-of-squares numbers))))
