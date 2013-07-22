;; Solution to Project Euler Problem 21

;; Evaluating the sum of all the amicable numbers under 10000

(defun divisor? (d x)
  "Return true if D is a proper divisor of X, false otherwise."
  (and (= 0 (mod x d))
       (/= d x)))

(defun d (x)
  "Return the sum of the divisors of X."
  (reduce #'+
          (loop for i from 1 to (1- x)
             when (divisor? i x)
             collect i)))

(defun collate-ds (n)
  "Produce an alist of pairs (x (d x)) for all x < N"
  (loop for i from 1 to (1- n)
       collecting (list i (d i))))

(defun order-within (al)
  "Given an alist AL of lists of numbers, sort each element of AL
such that its members are in order"
  (loop for p in al
       collecting (sort p #'<)))

(defun sequence-lessp (seq1 seq2)
  "Return T if seq1 is less than seq2"
  (loop
     for i in seq1
     for j in seq2
     when (< i j) return t
     when (< j i) return nil
     finally (return (<= i j))))

(defun order-without (al)
  "Given a list AL of lists of numbers, sort it according
to the lexicographic ordering of the lists."
  (sort al #'sequence-lessp))

(defun duplicates-recur (l)
  (when l
    (if (equal (car l)
               (cadr l))
        (cons (car l) (duplicates (cdr l)))
        (duplicates (cdr l)))))

(defun duplicates (l)
  "Given a sorted list L, return a list containing
one instance of each duplicated element"
  (remove-duplicates (duplicates-recur l) :test #'equal))

(defun inner-sum (l)
  (loop for e in l
       summing (reduce #'+ e)))

(defun problem-21 (n)
  (inner-sum
   (duplicates
    (order-without
     (order-within
      (collate-ds n))))))
