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

(defparameter *primes* (find-primes 10000))

(defclass triangle-number-generator ()
  ((running-sum :initform 0)
   (n           :initform 1)))

(defmethod next-triangle-number ((g triangle-number-generator))
  (setf (slot-value g 'running-sum)
        (+ (slot-value g 'n)
           (slot-value g 'running-sum)))
  (setf (slot-value g 'n) (+ (slot-value g 'n) 1))
  (slot-value g 'running-sum))

(defun factor? (f x)
  (= 0 (mod x f)))

(defun count-factors-incr (x curr so-far)
  (cond
    ((= x 1) (+ 1 so-far))
    ((factor? curr x)
     (count-factors-incr (/ x curr) curr (+ 1 so-far)))
    (t (count-factors-incr x (+ 1 curr) so-far))))

(defun count-factors-primes (x remaining-primes so-far)
  (let ((thisprime (car remaining-primes)))
    (cond
      ((= x 1) (+ 1 so-far))
      ((factor? thisprime x)
       (count-factors-primes (/ x thisprime) remaining-primes (+ 1 so-far)))
      ((consp (cdr remaining-primes))
       (count-factors-primes x (cdr remaining-primes) so-far))
      (t (count-factors-incr x (car remaining-primes) so-far)))))

(defun count-factors (x)
  (count-factors-primes x *primes* 2))

(defun problem-12 (n)
  "Return the value of the first triangle number to have over N
divisors."
  (let ((tgen (make-instance 'triangle-number-generator)))
    (do ((tnum (next-triangle-number tgen)
               (next-triangle-number tgen)))
        ((< n (count-factors tnum)) tnum))))
  
