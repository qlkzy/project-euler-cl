;; Solution to Project Euler Problem 2:

;; Summing the even-valued terms in the Fibonacci Sequence
;; Whose values are less than four million

(defun fibonacci-to (x)
  "Return a list of the terms in the Fibonacci Sequence
whose values are less than X."
  (let ((result nil))
    (do ((n-2 1 n-1)
         (n-1 1 (+ n-2 n-1)))
        ((> n-1 x) result)
      (setq result (cons n-1 result)))))

(defun problem-2 (n)
  (loop 
     for x in (fibonacci-to n)
     when (evenp x)
     sum x))
      
       
