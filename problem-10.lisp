;; Solution to Project Euler Problem 10

;; Finding the sum of the primes up to 2 million

(defun eliminate (sieve number)
  "Given a SIEVE (binary vector) and a NUMBER, set bits corresponding
to integer multiples of that number to 0."
  (loop 
     for i
     from (* number 2)                  ;second multiple
     below (length sieve)
     by number
     do (setf (sbit sieve i) 0)))

(defun do-sieving (sieve)
  "Run the Sieve of Erastothenes across a given bit-vector."
  (loop 
     for i
     from 2                             ;first prime
     to (sqrt (length sieve))           ;we can stop at sqrt(n)
     when (= 1 (sbit sieve i))
     do (eliminate sieve i)))

(defun find-primes (n)
  "Return a list of all the prime numbers less than N."
  (let ((sieve (make-array n
                           :element-type 'bit
                           :initial-element 1)))
    (do-sieving sieve)
    ;; collect numbers corresponding to set bits
    (loop for i from 2 to (- n 1)
       when (= 1 (sbit sieve i))
       collect i)))

(defun problem-10 (n)
  "Sum all the prime numbers less than N."
  (reduce #'+ (find-primes n)))
