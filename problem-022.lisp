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
  "Nonportably score a character for the Problem 22 rules."
  (+ 1
     (if (upper-case-p c)
         (- (char-code c) (char-code #\A))
         (- (char-code c) (char-code #\a)))))

(defun name-score (name)
  (loop for c across name
     sum (char-score c)))

(defun sorted (names)
  (sort names #'string-lessp))

(defun scores (names)
  (loop
     for n in names
     for i from 1
     summing (* i (name-score n))))

(defun problem-22 (stream)
  (scores
   (sorted
    (read-all-words stream))))
