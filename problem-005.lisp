;; Solution to Project Euler Problem 5

;; Find the smallest number evenly divisible by all the numbers from 1 to 20

(defparameter 1-to-20
  (loop for i from 1 to 20 collect i))

(defun multiple-of (n x)
  "Return true if X is a multiple of N, NIL otherwise."
  (eq 0 (mod x n)))

(defun multiple-of-many (fs x)
  "Return true if X is a multiple of all the elements of FS, NIL
otherwise."
  (every (lambda (f) (multiple-of f x))
         fs))

(defun problem-5 (fs)
  (loop for x from 20 by 20
       when (multiple-of-many fs x)
       return x))
