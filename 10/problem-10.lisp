;; Solution to Project Euler Problem 10

;; Finding the sum of the primes up to 2 million

(defun multiple-of (n x)
  "Return true if X is a multiple of N, NIL otherwise."
  (eq 0 (mod x n)))

(defun multiple-of-none (ns x)
  "Return true if X is a multiple of none of the elements of NS, NIL
otherwise."
  (loop for f in ns
     never (multiple-of f x)))

(defun next-prime (lastprime primes)
  "Given a list of PRIMES and the LASTPRIME (greatest prime), produce
the next prime."
  (do ((x (+ lastprime 2)
          (+ x 2)))
      ((multiple-of-none primes x) x)))

(defun primes-upto (n)
  "Generate a list of the primes up to N."
  (do ((lastprime 5 (next-prime lastprime primes))
       (primes '(2 3)
               (append primes (list lastprime))))
      ((> lastprime n) primes)))

(defun problem-10 (n)
  (apply #'+ (primes-upto n)))
  
