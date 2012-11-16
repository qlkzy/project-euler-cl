;; Solution to Project Euler Problem 2:
;; Summing the even-valued terms in the Fibonacci Sequence
;; Whose values are less than four million

(defun fib-succ (n-2 n-1)
  "Return the Nth term in the Fibonacci Sequence,
given the values of the N-2th and N-1th terms."
  (+ n-2 n-1))

(defun fibonacci-to (x)
  "Return a list of the values in the Fibonacci Sequence
whose values are less than X."
  (let ((result nil))
    (do ((n-2 1 n-1)
         (n-1 1 (fib-succ n-2 n-1)))
        ((> n-1 x) result)
      (setq result (cons n-1 result)))))

(defun x-if-even (x)
  "Return X if X is even, 0 otherwise."
  (if (evenp x)
      x
      0))

(defun problem-2 (n)
  (apply #'+
         (loop for x in (fibonacci-to n)
              collect (x-if-even x))))
      
       
