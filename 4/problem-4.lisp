;; Solution to Project Euler Problem 3
;; 
;; Finding the largest palindrome made from the product of 2 3-digit
;; numbers

(defun palindrome-p (l)
  "Return true if L is palindromic; false otherwise."
  (equal l
         (reverse l)))

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

(defun palindromic-number-p (x)
  "Return true if X is palindromic, false otherwise."
  (palindrome-p (digits x)))

(defun all-n-digit-numbers (n)
  "Return a list of all the N-digit numbers, in order."
  (loop for i from 1 to (1- (expt 10 n)) collect i))

(defun all-products-of-n-digit-numbers (n)
  "Return a list of all the products of all the N-digit numbers."
  (let ((numbers (all-n-digit-numbers n)))
     (loop for x in numbers
        appending (loop for y in numbers collect (* x y)))))

(defun all-palindromic-products-of-n-digit-numbers (n)
  (remove-if
   (complement #'palindromic-number-p)
   (all-products-of-n-digit-numbers n)))

(defun largest (l)
  "Return the largest member of the list L."
  (car (sort l #'>)))

(defun problem-4 (n)
  "Return the largest palindromic product of 2 n-digit-numbers."
  (largest (all-palindromic-products-of-n-digit-numbers n)))
