;; Solution to Project Euler Problem 16

;; Find the sum of the digits in 2^1000

(defun lowest-digit (x)
  "Return the lowest digit of X."
  (mod x 10))

(defun nth-digit (n x)
  "Return the Nth digit of X."
  (lowest-digit (truncate (/ x (expt 10 n)))))

(defun count-digits (x)
  "Return the number of digits in the decimal representation of X."
  (1+ (floor (log x 10))))

(defun digits (x)
  "Return a list containing the digits of in the decimal
representation of X, in order."
  (loop for i from (1- (count-digits x)) downto 0
     collect (nth-digit i x)))

(defun problem-16 (n)
  "Find the sum of the digits in 2^n."
  (reduce #'+
          (digits (expt 2 n))))
