;; Solution to Project Euler Problem 35

;; Finding all circular primes below of one million

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
    (loop for i from 2 below n
       when (= 1 (sbit sieve i))
       collect i)))

(defun make-sieve (n)
  (let ((sieve (make-array n
                           :element-type 'bit
                           :initial-element 1)))
    (do-sieving sieve)
    sieve))

(defun list-primes (sieve n)
  (loop for i from 2 below n
     when (= 1 (sbit sieve i))
     collect i))

(defun ndigits (x)
  (1+ (floor (log x 10))))

(defun rot (x digits)
  (+ (floor x 10)
     (* (mod x 10)
        (expt 10 (1- digits)))))

(defun rotations (x)
  (let ((digits (ndigits x)))
    (loop for i from 1 to digits
       for r = x then (rot r digits)
       collecting r)))

(defun rotations-prime (x primes)
  (loop for r in (rotations x)
       always (= 1 (sbit primes r))))

(defun problem-35 (n)
  (let* ((sieve (make-sieve (* n 10)))
        (primes (list-primes sieve n)))
    (loop for p in primes
       while (< p 1000000)
       when (rotations-prime p sieve)
       count p)))
