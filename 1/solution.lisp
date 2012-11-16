;; Solution to Project Euler Problem 1:
;; Summing all the natural numbers below 1000
;; that are multiples of 3 or 5

(defun multiple-of (n x)
  "Return true if X is a multiple of N, NIL otherwise."
  (eq 0 (mod x n)))

(defun multiple-of-3-or-5 (x)
  "Return true if X is a multiple of 3 or 5, NIL otherwise."
  (or (multiple-of 3 x)
      (multiple-of 5 x)))

(defun x-if-multiple-of-3-or-5 (x)
  "Return X if X is a multiple of 3 or 5, 0 otherwise."
  (if (multiple-of-3-or-5 x)
      x
      0))

(defun solve (n)
  "Return the sum of the natural numbers below N
which are multiples of 3 or 5."
  (apply #'+ 
         (loop for x from 1 to (- n 1)
            collect (x-if-multiple-of-3-or-5 x))))
  
