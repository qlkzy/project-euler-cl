;; Solution to Project Euler Problem 12

;; Find the value of the first triangle number to have over 500
;; divisors

(defun divisor? (d x)
  "Return true if D is a divisor of X, false otherwise."
  (= 0 (mod x d)))

(defun mark-divisors (sieve d x)
  "Mark the divisors of X implied by the fact that D is a divisor into
SIEVE."
  (setf (sbit sieve d) 1)
  (setf (sbit sieve (floor (/ x d))) 1))

(defun count-divisors (sieve)
  "Count the number of divisors marked into SIEVE."
  (count 1 sieve))

(defun n-divisors (x)
  "Return the number of divisors of X."
  (let ((sieve (make-array (1+ x) :element-type 'bit :initial-element 0)))
    (loop for i from 1 to (sqrt x)
       when (divisor? i x)
       do (mark-divisors sieve i x))
    (count-divisors sieve)))

(defclass triangle-number-generator ()
  ((running-sum :initform 0)
   (n           :initform 1)))

(defmethod next-triangle-number ((g triangle-number-generator))
  (setf (slot-value g 'running-sum)
        (+ (slot-value g 'n)
           (slot-value g 'running-sum)))
  (setf (slot-value g 'n) (+ (slot-value g 'n) 1))
  (slot-value g 'running-sum))

(defun problem-12 (n)
  "Return the value of the first triangle number to have over N
divisors."
  (let ((tgen (make-instance 'triangle-number-generator)))
    (do ((tnum (next-triangle-number tgen)
               (next-triangle-number tgen)))
        ((< n (n-divisors tnum)) tnum))))
  
