;; Solution to Project Euler problem 7

;; Finding the 10001st prime

(defun multiple-of (n x)
  "Return true if X is a multiple of N, NIL otherwise."
  (eq 0 (mod x n)))

(defun multiple-of-none (ns x)
  "Return true if X is a multiple of none of the elements of NS, NIL
otherwise."
  (loop for f in ns
     never (multiple-of f x)))

(defun next-prime (primes)
  "Given a list of primes (in reverse-sorted order), produce the next
prime."
  (do ((x (1+ (car primes))
          (1+ x)))
      ((multiple-of-none primes x) x)))

(defun nth-prime (n)
  (do ((i 1 (1+ i))
       (primes '(2) 
               (cons (next-prime primes) primes)))
      ((= i n) (car primes))))
