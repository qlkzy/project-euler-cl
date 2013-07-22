(defun terms (a b)
  (loop for i from 2 to a appending
       (loop for j from 2 to b collecting
            (expt i j))))

(defun problem-29 (a b)
  (length
   (remove-duplicates
    (terms a b))))
