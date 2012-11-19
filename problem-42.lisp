;; Solution to Project Euler Problem 42

;; Counting 'Triangle Words' in a text file

(defun read-word (stream)
  ;; consume commas and quotes
  (do ((c (peek-char t stream) (peek-char t stream)))
      ((not (or (equal c #\")
                (equal c #\,))))
    (read-char stream))
  ;; read a word
  (do ((c (read-char stream) (read-char stream))
       (word (make-array 0
                         :element-type 'character
                         :fill-pointer 0
                         :adjustable t)))
      ((equal c #\") word)
    (vector-push-extend c word)))

(defun maybe-read-word (stream)
  (handler-case
   (read-word stream)
   (end-of-file () nil)))

(defun read-all-words (stream)
  (loop for w = (maybe-read-word stream)
     until (not w)
     collecting w))

(defun char-score (c)
  "Nonportably score a character for the Problem 42 rules."
  (+ 1
     (if (upper-case-p c)
         (- (char-code c) (char-code #\A))
         (- (char-code c) (char-code #\a)))))

(defun word-score (word)
  (loop for c across word
       sum (char-score c)))

(defun triangle? (number)
  (= number
     (do ((n 1 (+ n 1))
          (tri 0 (+ tri n)))
         ((<= number tri) tri))))

(defun problem-42 (stream)
  (loop for w in (read-all-words stream)
       count (triangle? (word-score w))))
