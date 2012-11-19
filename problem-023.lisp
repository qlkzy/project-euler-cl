;; Solution to Project Euler Problem 23

;; Finding the sum of all positive integers which cannot be written as
;; the sum of two abundant numbers


(defun divisor? (d x)
  "Return true if D is a divisor of X."
  (= 0 (mod x d)))

(defun divisors (x)
  "Return a list of the divisors of X (not including X)."
  (loop for d from 1 to (/ x 2)
       when (divisor? d x)
       collect d))

(defun abundant? (x)
  "Return T if X is abundant, NIL otherwise."
  (< x (reduce #'+ (divisors x))))

(defun bv-to-n (n i)
  "Return a bitvector of length N, with all elements having initial
value I."
  (make-array (+ 1 n) :element-type 'bit :initial-element i))

(defun abundant-numbers (n)
  "Return a bitvector of length N+1, with values corresponding to
abundant numbers set to 1 and all other values set to 0."
  (let ((nums (bv-to-n n 0)))
    (loop for x from 1 to n
       when (abundant? x)
       do (setf (sbit nums x) 1))
    nums))

(defun not-from-sum-abundant (n)
  "Return a list of all the positive integers less than or equal to N
which cannot be written as the sum of two abundant numbers."
  (let ((abundants (abundant-numbers n))
        (ok (bv-to-n n 0)))
