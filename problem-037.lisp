

(defclass prime-generator ()
  ((primes :initform '(2 3 5 7))))

(defgeneric next-prime (gen))
(defgeneric prime-p (gen p))

(defmethod next-prime ((gen prime-generator))
  (let ((next (loop for i from (1+ (apply #'max (slot-value gen 'primes)))
                 when (loop for p in (slot-value gen 'primes)
                          always (/= 0 (mod i p)))
                 return i)))
    (setf (slot-value gen 'primes)
          (append (slot-value gen 'primes) (list next)))
    next))

(defmethod prime-p ((gen prime-generator) p)
  (member p (slot-value gen 'primes)))

(defun left-truncate (x)
  (mod x (expt 10 (floor (log x 10)))))

(defun right-truncate (x)
  (floor x 10))

(defun left-truncations (x)
  (loop for n = x then (left-truncate n)
     until (= n 0)
     collect n))

(defun right-truncations (x)
  (loop for n = x then (right-truncate n)
     until (= n 0)
     collect n))

(defun truncations (x)
  (append (left-truncations x)
          (right-truncations x)))

(defun all-truncations-prime (gen x)
  (loop for y in (truncations x)
       always (prime-p gen y)))

(defun problem-37 ()
  (reduce
   #'+
   (let ((gen (make-instance 'prime-generator)))
     (loop for p = (next-prime gen)
        with count = 0
        until (>= count 11)
        when (all-truncations-prime gen p)
        collect p and do (incf count)))))
