;; Solution to Project Euler problem 7

;; Finding the 10001st prime

;; Hilariously inefficient

(defun multiple-of (f x)
  "Return true if X is a multiple of F, NIL otherwise."
  (eq 0 (mod x f)))

(defun multiple-of-none (fs x)
  "Return true if X is a multiple of none of the elements of FS, NIL
otherwise."
  (notany (lambda (f) (multiple-of f x))
          fs))

(defun next-prime (primes)
  "Given a list of primes (in reverse-sorted order), produce the next
prime."
  (loop
     for p from (+ 1 (car primes))
     when (multiple-of-none primes p)
     return p))

(defun nth-prime (n)
  (loop
     for i from 1 to n
     for primes = '(2) then (cons (next-prime primes) primes)
     finally (return (first primes)))))
