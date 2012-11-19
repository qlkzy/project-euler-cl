;; Solution to Project Euler Problem 3:
;; Finding the Largest Prime Factor of 600851475143

(defconstant problem-3-number 600851475143)

(defun factor? (f x)
  "Return true if F is a factor of X, NIL otherwise."
  (= 0 (mod x f)))

(defun trim-by-factor (f x)
  "If F is a factor of X, return X/F; otherwise return X."
  (if (factor? f x)
      (/ x f)
      x))

(defun next-possible-factor (f x)
  "Return F if F is a factor of X; otherwise return the next possible
factor of X."
  (if (factor? f x)
      f
      (+ f 1)))

(defun problem-3 (n)
  "Return a list of the prime factors of N."
  (let ((factors nil))
    (do ((f 2 (next-possible-factor f x))
         (x n (trim-by-factor f x)))
        ((= x 1) factors)
        (when (factor? f x)
            (push f factors)))))
         
