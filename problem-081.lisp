
(load "problem-081-matrix.lisp")

;; (defparameter input-data
;;   '((131 673 234 103 18)
;;     (201 96 342 965 150)
;;     (630 803 746 422 111)
;;     (537 699 497 121 956)
;;     (805 732 524 37	331)))

(defparameter matrix
  (make-array (list (length input-data)
                    (length (car input-data)))
              :initial-contents input-data))

(defparameter costs
  (make-array (array-dimensions matrix)
              :initial-element 0))

(defun nrows (mat)
  (car (array-dimensions mat)))

(defun ncols (mat)
  (cadr (array-dimensions mat)))

(defun cost-to (r c)
  (cond
    ((and (= r 0) (= c 0)) 0)
    ((= r 0) (aref costs r (1- c)))
    ((= c 0) (aref costs (1- r) c))
    (t (min (aref costs (1- r) c)
            (aref costs r (1- c))))))

(defun problem-081 ()
  (loop for a from 0 below (nrows matrix)
     do (loop for r from a below (nrows matrix)
           do (setf (aref costs r a)
                    (+ (cost-to r a)
                       (aref matrix r a))))
     do (loop for c from a below (nrows matrix)
           do (setf (aref costs a c)
                    (+ (cost-to a c)
                       (aref matrix a c)))))
  (aref costs
        (1- (nrows costs))
        (1- (ncols costs))))
