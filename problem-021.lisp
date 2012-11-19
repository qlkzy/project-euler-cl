;; Solution to Project Euler Problem 21

;; Evaluating the sum of all the amicable numbers under 10000

(defun divisor? (d x)
  "Return true if D is a proper divisor of X, false otherwise."
  (and (= 0 (mod x d))
       (/= d x)))

(defun sum-divisors (x)
  "Return the number of divisors of X."
  (reduce #'+
          (loop for i from 1 to (1- x)
             when (divisor? i x)
             collect i)))

(defun collate-divisors (n)
  "Produce a list of cons cells containing in the car a number from 1
to n, and in the cdr the sum of the divisors of that number."
  (loop for i from 1 to n
       collect (cons i (sum-divisors i))))

(defun xref-divisors (n)
  (let ((forward-map (make-array (list (* 2 n))
                                 :initial-element 0))
        (reverse-map (make-array (list (* 2 n))
                                 :initial-element 0)))
    (loop for x in (collate-divisors n) do
         (progn (setf (aref forward-map (car x)) (cdr x))
                (setf (aref reverse-map (cdr x)) (car x))))
    (loop for i from 0 below (length forward-map)
         when (= i (aref reverse-map (aref forward-map i)))
         collect (cons i (aref forward-map i)))))
                 
         
