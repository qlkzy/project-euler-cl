;; Solution to Project Euler Problem 9

;; Find the product of the members of the pythagorean triplet whose
;; members sum to 1000

(defun triplet-p (a b c)
  "Return T if A, B, and C form a pythagorean triplet; NIL otherwise"
  (= (* c c)
     (+ (* a a)
        (* b b))))

(defun sum-to-n-p (n a b c)
  "Return T if A, B, and C sum to N; NIL otherwise."
  (= n
     (+ a b c)))

(defun triplets-up-to-n (n)
  "Generate pythagorean triplets whose members sum to N."
  (loop for a from 1 to n appending
       (loop for b from 1 to n appending
            (loop for c from 1 to n
               when (and (triplet-p a b c)
                         (sum-to-n-p n a b c))
               collect (list a b c)))))

(defun problem-9 (n)
  "Find the products of the pythagorean triplets whose members sum to
N."
  (mapcar (lambda (x) (apply #'* x))
          (triplets-up-to-n n)))
