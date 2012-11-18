;; Solution to Project Euler Problem 28

;; Finding the sum of the diagonals on a clockwise spiral

(defun nth-corners (edge prev)
  "Gets the numbers of the four corners of the spiral, given the EDGE
length of this ring and the ending number of the PREV ring.

Returns the result in reversed order to allow easily finding the
subsequent value for PREV."
  (loop for i from 4 downto 1
       collect (+ (* i (- edge 1)) prev)))

(defun diagonals (edge)
  "Gets all the numbers on the diagonals of a clockwise spiral of edge
length EDGE."
  (do ((e 3 (+ e 2))
       (d '(1) (append (nth-corners e (first d)) d)))
      ((< edge e) d)))

(defun problem-28 (n)
  "Find the sum of the numbers on the diagonals on a N by N spiral."
  (reduce #'+ (diagonals n)))
