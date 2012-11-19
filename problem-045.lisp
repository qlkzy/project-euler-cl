;; Solution to Project Euler Problem 45

;; Finding a number which is Triangular, Pentagonal, and Hexagonal

(defclass sided-number-generator ()
  ;; need a very fast hashtable to make this work
  ((cache :initform (make-hash-table :test #'eq))
   (largest-number :initform 0)
   (largest-n :initform 0)))

(defmethod update-nth ((gen sided-number-generator) n)
  (let ((x (calculate-nth gen n)))
    (setf (gethash x (slot-value gen 'cache)) x
          (slot-value gen 'largest-number) x
          (slot-value gen 'largest-n) n)))

(defmethod nagonal? ((gen sided-number-generator) x)
  (when (< (slot-value gen 'largest-number) x)
    (loop for n
       from (+ (slot-value gen 'largest-n) 1)
       until (> (slot-value gen 'largest-number) x)
       do (update-nth gen n)))
  (gethash x (slot-value gen 'cache)))

(defclass triangle-number-generator (sided-number-generator) ())
(defclass pentagonal-number-generator (sided-number-generator) ())
(defclass hexagonal-number-generator (sided-number-generator) ())

(defmethod calculate-nth ((g triangle-number-generator) n)
  (floor (* n (/ (+ n 1) 2))))

(defmethod calculate-nth ((g pentagonal-number-generator) n)
  (floor (* n (/ (- (* 3 n) 1) 2))))

(defmethod calculate-nth ((g hexagonal-number-generator) n)
  (* n (- (* 2 n) 1)))
      
(defun problem-45 (n)
  "Find the first number greater than N which is triangular,
pentagonal, and hexagonal."
  (let ((tri (make-instance 'triangle-number-generator))
        (pen (make-instance 'pentagonal-number-generator))
        (hex (make-instance 'hexagonal-number-generator)))
    (loop for x from n
         when (and (nagonal? tri x)
                   (nagonal? pen x)
                   (nagonal? hex x))
         return x)))
