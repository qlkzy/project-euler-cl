

(defclass prime-generator ()
  ((primes :initform '(2))))

(defparameter foo
  (make-instance 'prime-generator))

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
