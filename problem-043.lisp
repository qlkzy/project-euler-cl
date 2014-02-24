(defparameter digits
  (loop for i from 0 to 9 collecting i))

(defun pd-suffixes (num)
  (loop for e in (set-difference digits num)
     collecting (append num (list e))))

(defun pd-suffix-n (num n)
  (if (= n 0)
      (list num)
      (loop for x in (pd-suffixes num)
         append (pd-suffix-n x (1- n)))))

(defun numeric-last (num n)
  (do ((e (expt 10 (1- n)) (floor e 10))
       (ds (last num n) (cdr ds))
       (acc 0 (+ acc (* e (car ds)))))
      ((null ds) acc)))

(defun suffix-divisible (divisor num)
  (remove-if-not 
   (lambda (x)
     (= 0 (mod (numeric-last x 3) divisor)))
   (pd-suffixes num)))

(defun suffixes-divisible (divisor nums)
  (loop for n in nums
     appending (suffix-divisible divisor n)))

(defun problem-43 ()
  (loop for x in (suffixes-divisible
                  17
                  (suffixes-divisible
                   13
                   (suffixes-divisible
                    11
                    (suffixes-divisible
                     7
                     (suffixes-divisible
                      5
                      (suffixes-divisible
                       3
                       (suffixes-divisible
                        2
                        (pd-suffix-n '() 3))))))))
       summing (numeric-last x 10)))
