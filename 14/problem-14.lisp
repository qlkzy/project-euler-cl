;; Solution to Project Euler Problem 14

;; Finding the longest hailstone sequence for starting numbers under
;; 1000000

(defvar *hailstone-cache*
  (make-array 1000000
              :element-type '(unsigned-byte 32)
              :initial-element 0)
  "Slightly horrible global cache for lengths of sequences")

(defun hailstone-succ (n)
  "Return the successor to N in the hailstone sequence."
  (if (evenp n)
      (floor (/ n 2))
      (+ (* 3 n) 1)))

(defun hailstone-insert (n)
  "Insert the count for term N into the cache"
  (setf (aref *hailstone-cache* n)
        (count-hailstone-terms n)))

(defun hailstone-cached? (n)
  "Returns true if the term count for N is cached, false otherwise."
  (/= 0 (aref *hailstone-cache* n)))

(defun hailstone-within-cache? (n)
  "Returns true if our cache extends far enough to store this value."
  (< n (array-dimension *hailstone-cache* 0)))

(defun hailstone-get-cached (n)
  "Retrieves a cached value or inserts a new value into the cache"
  (if (hailstone-cached? n)
      (aref *hailstone-cache* n)
      (hailstone-insert n)))

(defun hailstone-get (n)
  "Gets the term count for N, using the cache if possible."
  (if (hailstone-within-cache? n)
      (hailstone-get-cached n)
      (count-hailstone-terms n)))

(defun count-hailstone-terms (n)
  "Recursively count the number of terms in the hailstone sequence
starting with N."
  (if (= 1 n)
      1
      (+ (hailstone-get (hailstone-succ n)) 1)))

(defun problem-14 (n)
  (reduce (lambda (a b)
            (if (> (car a) (car b))
                   a b))
          (loop for i from 1 to n collect
               (list (count-hailstone-terms i) i))))

