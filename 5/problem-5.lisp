;; Solution to Project Euler Problem 5

;; Find the smallest number evenly divisible by all the numbers from 1 to 20

(defconstant 1-to-20
  (loop for i from 1 to 20 collect i))

(defun multiple-of (n x)
  "Return true if X is a multiple of N, NIL otherwise."
  (eq 0 (mod x n)))

(defun multiple-of-many (ns x)
  "Return true if X is a multiple of all the elements of NS, NIL
otherwise."
  (loop for f in ns
       always (multiple-of f x)))

(defun problem-5 (ns)
  (do ((x 20 (+ x 20)))
      ((multiple-of-many ns x) x)))
