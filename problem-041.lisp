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

(defun make-sieve (n)
  (let ((sieve (make-array n
                           :element-type 'bit
                           :initial-element 1)))
    (do-sieving sieve)
    sieve))

(defparameter digits
  '(1 2 3 4 5 6 7 8 9 0))

(defparameter massive-sieve
  (make-sieve 1000000000))

(defparameter small-sieve
  (make-sieve 10000))

(defun ndigits (x)
  (1+ (floor (log x 10))))

(defun pandigital (x)
  (let ((n (ndigits x))
        (ds (loop
               for y = x then (floor y 10)
               for d = (mod y 10)
               until (= y 0)
               collecting d)))
    (and (= (/ (* n (+ n 1)) 2)
            (apply #'+ ds))
        (null (set-difference (subseq digits 0 n) ds)))))

(defun problem-041 (sieve)
  (loop for i downfrom (1- (car (array-dimensions sieve))) downto 1
     when (and (= 1 (sbit sieve i))
               (pandigital i))
     return i))
