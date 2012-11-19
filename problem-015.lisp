;; Solution for Project Euler Problem 15

;; Counting the number of routes through a 20x20 grid.

(defun from-left (grid row col)
  "Get the number of routes on GRID from the left to the cell at (COL,ROW)."
  (if (= col 0)
      0
      (aref grid row (1- col))))

(defun from-above (grid row col)
  "Get the number of routes on GRID from above to the cell at (COL,ROW)."
  (if (= row 0)
      0
  (aref grid (1- row) col)))

(defun calculate (grid row col)
  "Calculate the number of routes on GRID to the cell at (COL,ROW),
and store the calculated value in GRID."
  (if (= row col 0)
      1
      (setf (aref grid row col)
            (+ (from-left grid row col)
               (from-above grid row col)))))

(defun problem-15 (side)
  "Calculates the number of non-backtracking paths from the top left
to the bottom right of a square grid of side length SIDE."
  (let ((grid (make-array (list (1+ side) (1+ side))
                          :initial-element 1)))
    (loop for row from 0 to side do
         (loop for col from 0 to side do
              (calculate grid row col)))
    (aref grid side side)))
