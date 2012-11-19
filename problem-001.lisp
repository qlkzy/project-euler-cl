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

(defun problem-1 (n)
  "Return the sum of the natural numbers below N
which are multiples of 3 or 5."
  (apply #'+ 
         (loop for x from 1 to (- n 1)
            collect (x-if-multiple-of-3-or-5 x))))
  

;;; alternative implementation

(defun factorp (f x)
  "Return true if F is a factor of X."
  (= 0 (mod x f)))
  
(defun problem-1-concise (n)
  "Return the sum of the natural numbers below N
which are multiples of 3 or 5."
  (loop for x from 1 below n
     when (or (factorp 3 x)
              (factorp 5 x))
     summing x))
