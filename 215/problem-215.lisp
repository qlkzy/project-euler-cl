;; Solution to Project Euler Problem 215

;; Forming crack-free walls

(defun prepend-forall (item lists)
  "Prepend ITEM to all of the sequences in LISTS."
  (mapcar (lambda (l) (cons item l))
          lists))

(defun rows-of-length (n)
  "Return a list of all the lists of brick lengths that form a row of
length N."
  (cond
    ((= n 0) nil)
    ((= n 1) nil)
    ((= n 2) '((2)))
    ((= n 3) '((3)))
    (t (append (prepend-forall 2 (rows-of-length (- n 2)))
               (prepend-forall 3 (rows-of-length (- n 3)))))))

(defun crack-indices (row)
  "Return a list of the indexes into ROW (a list of brick lengths) at
which cracks occur."
  (butlast
   (loop
      for b in row
      for c = b then (+ c b)
      collecting c)))

(defun empty-crack-vector (length)
  "Return an empty crack vector (no cracks) for a wall of length
LENGTH."
  (make-array length
              :element-type 'bit
              :initial-element 0))

(defun crack-vector (row)
  "Return a binary vector indicating the positions of cracks in ROW."
  (let ((cracks (empty-crack-vector (reduce #'+ row))))
    (loop for c in (crack-indices row)
         do (setf (sbit cracks c) 1))
    cracks))

(defun matching-cracks? (row1 row2)
  "Return T if the rows represented by the bit-vectors ROW1 and
ROW2 have corresponding cracks; NIL otherwise."
  (/= 0
      (count 1
             (bit-and row1 row2))))


(defun check-row (row layer-below)
  "Test ROW against each row in LAYER-BELOW. Return a pair, with ROW
as the car, and the sum of the cdrs of the rows in LAYER-BELOW as
the cdr."
  (cons 
   row
   (loop for p in layer-below
      when (not (matching-cracks? row (car p)))
      summing (cdr p))))

(defun build-bottom-layer (length)
  (mapcar
   (lambda (x) (cons x 1))
   (possible-rows length)))

(defun possible-rows (length)
  (mapcar #'crack-vector (rows-of-length length)))

(defun build-wall (length height)
  (if (= height 1)
      (build-bottom-layer length)
      (let ((layer-below (build-wall length (- height 1))))
        (loop for r in (possible-rows length)
           collecting (check-row r layer-below)))))
       
(defun problem-215 (length height)
  (loop for w in (build-wall length height)
       summing (cdr w)))
