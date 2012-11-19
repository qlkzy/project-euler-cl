;; Solution to Project Euler Problem 55

;; Counting Lychrel numbers below ten thousand


(defun digits (x)
  "Return a list containing the digits of in the decimal
representation of X, in order."
  (loop for i from (floor (log x 10)) downto 0
     collect (mod (truncate (/ x (expt 10 i))) 10)))

(defun reversed-digits (x)
  "Return the value of the number with the same digits as X, but in
reversed order."
  (let ((result 0)
        (ndigits (floor (log x 10))))
    (loop for i from ndigits downto 0
         do (setf result
                  (+ result (* (mod (truncate (/ x (expt 10 i))) 10)
                               (expt 10 (- ndigits i))))))
    result))

(defun not-lychrel? (x)
  (loop 
     for fwd = x then (+ fwd rev)
     for rev = (reversed-digits fwd)
     for its from 1
     when (> its 50)
     return nil
     thereis (and (not (= fwd x))
                       (= fwd rev))))

(defun lychrel? (x)
  (not (not-lychrel? x)))
       
(defun problem-55 (n)
  "Count the Lychrel numbers below N."
  (loop for i from 1 to n 
     count (lychrel? i)))
