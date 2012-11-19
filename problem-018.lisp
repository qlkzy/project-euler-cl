;; Solution to Projcet Euler problem 18

;; Finding maximum-weight routes down a triangle

(defconstant problem-18-triangle
  (make-array '(120)
              :initial-contents
              '(75
                95 64
                17 47 82
                18 35 87 10
                20 04 82 47 65
                19 01 23 75 03 34
                88 02 77 73 07 63 67
                99 65 04 28 06 16 70 92
                41 41 26 56 83 40 80 70 33
                41 48 72 33 47 32 37 16 94 29
                53 71 44 65 25 43 91 52 97 51 14
                70 11 33 28 77 73 17 78 39 68 17 57
                91 71 52 38 17 14 91 43 58 50 27 29 48
                63 66 04 68 89 53 67 30 73 16 69 87 40 31
                04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))

(defun row-col-to-index (row col)
  "Convert from ROW, COLumn addressing to index addressing."
  (+ col
     (reduce #'+ (loop for i from 0 to row collect i))))

(defun index-to-row-col (index)
  "Convert from INDEX addressing to row, column addressing."
  (values
   (- (do ((row 0 (+ row 1)))
          ((> row index) row)
        (setq index (- index row)))
      1)
   index))

(defun right-child-index (index)
  "Get the index of the right child of the cell at INDEX."
  (multiple-value-bind (row) (index-to-row-col index)
    (+ index row 2)))

(defun left-child-index (index)
  "Get the index of the left child of the cell at INDEX."
  (multiple-value-bind (row) (index-to-row-col index)
    (+ index row 1)))

(defun compute-value (orig grid index)
  "Calculate the value that should go in a cell, based on:
    The ORIGinal triangle grid.
    The memoized GRID of the computation thus far.
    The INDEX of the cell."
  (if (>= (index-to-row-col index)
          (index-to-row-col (1- (length grid))))
      (aref orig index)
      (+ (aref orig index)
         (max (cell-value orig grid (left-child-index index))
              (cell-value orig grid (right-child-index index))))))

(defun cell-value (orig grid index)
  "Return the value in a cell, using the cache where possible."
  (if (/= 0 (aref grid index))
      (aref grid index)
      (setf (aref grid index)
            (compute-value orig grid index))))
  
(defun problem-18 ()
  (let ((grid (make-array (list (length problem-18-triangle))
                          :initial-element 0)))
    (cell-value problem-18-triangle
                grid
                0)))
