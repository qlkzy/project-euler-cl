;; Solution to Project Euler Problem 40

;; Finding digits from the sequence formed by concatenating the
;; positive integers

;; horrifically inefficient, but fast enough PE

(defun digits (x)
  "Return a list containing the digits of in the decimal
representation of X, in order."
  (loop for i from (floor (log x 10)) downto 0
     collect (mod (truncate (/ x (expt 10 i))) 10)))

(defun concat-integers (n)
  (loop for i from 1 to n appending
       (digits i)))

(defun dn (subscripts)
  (let ((seq (concat-integers (reduce #'max subscripts))))
    (loop for s in subscripts collect
         (nth (- s 1) seq))))

(defun problem-40 ()
  (reduce #'*
          (dn '(1
                10
                100
                1000
                10000
                100000
                1000000))))
