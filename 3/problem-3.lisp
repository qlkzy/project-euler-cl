;; Solution to Project Euler Problem 3:
;; Finding the Largest Prime Factor of 600851475143

(defconstant problem-3-number 600851475143)

(defun multiple-of (n x)
  "Return true if X is a multiple of N, NIL otherwise."
  (eq 0 (mod x n)))

(defun trim-by-factor (f x)
  "If F is a factor of X, return X/F; otherwise return X."
  (if (multiple-of f x)
      (/ x f)
      x))

(defun next-possible-factor (f x)
  "Return F if F is a factor of X; otherwise return the next possible
factor of X."
  (if (multiple-of f x)
      f
      (+ f 1)))

(defun problem-3 (n)
  "Return a list of the prime factors of N."
  (let ((factors nil))
    (do ((f 2 (next-possible-factor f x))
         (x n (trim-by-factor f x)))
        ((= x 1) factors)
        (when (multiple-of f x)
            (setq factors (cons f factors))))))
         
