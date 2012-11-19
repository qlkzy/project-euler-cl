;; Solution to Project Euler Problem 12

;; Find the value of the first triangle number to have over 500
;; divisors

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

(defparameter *primes* (find-primes 1000000))

(defun factor? (f x)
  (= 0 (mod x f)))

(defun push-factor (f l)
  (cond
    ((and (consp (car l))
          (= (caar l)
             f))
     (cons (cons (caar l)
                 (+ (cdar l) 1))
           (cdr l)))
    (t (cons (cons f 1) l))))

(defun prime-factors-impl (x remaining-primes so-far)
  (let ((thisprime (car remaining-primes)))
    (cond
      ((= x 1) so-far)
      ((factor? thisprime x)
       (prime-factors-impl (/ x thisprime) remaining-primes (push-factor thisprime so-far)))
      (t (prime-factors-impl x (cdr remaining-primes) so-far)))))

(defun prime-factors (x)
  (prime-factors-impl x *primes* '()))

(defun ndivs (factors)
  (reduce #'*
          (mapcar (lambda (x) (+ 1 (cdr x)))
                  factors)))

(defun n-divisors (x)
  (ndivs (prime-factors x)))

(defun nth-triangle (n)
  (* n (/ (+ 1 n) 2)))

(defun problem-12 (n)
  "Return the value of the first triangle number to have over N
divisors."
  (loop 
     for i from 1
     for x = (nth-triangle i)
     when (< n (n-divisors x))
     return x))
  
