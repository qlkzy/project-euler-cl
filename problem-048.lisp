(defun problem-489 ()
  (mod (loop
          for i from 1 to 1000
          summing (expt i i))
       (expt 10 10)))
