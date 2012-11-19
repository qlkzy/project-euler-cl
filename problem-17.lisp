;; Solution to Project Euler Problem 17

;; Counting the number of letters used to write out the numbers from 1
;; to 1000 as words

(defconstant multiples-words
  '((1000 . thousand) (100 . hundred)))

(defconstant following-words
  '((90 . ninety) (80 . eighty) (70 . seventy) (60 . sixty)
    (50 . fifty) (40 . forty) (30 . thirty) (20 . twenty)
    (19 . nineteen) (18 . eighteen) (17 . seventeen) (16 . sixteen)
    (15 . fifteen) (14 . fourteen) (13 . thirteen) (12 . twelve)
    (11 . eleven) (10 . ten) (9 . nine) (8 . eight) (7 . seven)
    (6 . six) (5 . five) (4 . four) (3 . three) (2 . two) (1 . one)))

(defconstant following-words-symbols-only
  (mapcar (lambda (x) (cdr x)) following-words))

(defconstant numbers-words
  (concatenate 'list multiples-words following-words))

(defun pick-number-word (n)
  "Given a number N, choose the largest number word which fits into N,
and return a cons cell containing the word as the cdr and
N-(value-of-the-word) as the car."
  (loop for e in number-words
       when (<= (car e) n)
       return (cons (- n (car e))
                    (cdr e))))

(defun get-number-words (n)
  "Get a list of words corresponding to the number N.

In this representation, multiples are represented by multiple
ocurrences of the word - e.g. 200 is HUNDRED HUNDRED."
  (let ((nw (pick-number-word n)))
    (when nw
      (cons (cdr nw)
            (get-number-words (car nw))))))

(defun rewrite-and (word successors wordlist)
  "Rewrite occurrences of WORD SUCCESSORS in WORDLIST to look like
WORD AND SUCCESSOR.

We assume that there is only one contiguous run of WORD."
  (let ((start (position word wordlist :from-end t)))
    (if start
        (concatenate
         'list
         (subseq wordlist 0 (1+ start))
         (if (and (< start (length wordlist))
                  (some (lambda (x) (eq x (nth (1+ start) wordlist)))
                        successors))
             '(and))
         (subseq wordlist (1+ start)))
        wordlist)))
         
(defun rewrite-multiples (word wordlist)
  "Rewrite multiple ocurrences of WORD in WORDLIST into the form <count> WORD.

E.G. rewrite HUNDRED HUNDRED into TWO HUNDRED."
  (let ((start (position word wordlist))
        (end (position word wordlist :from-end t)))
    (if start
        (concatenate
         'list
         (or (subseq wordlist 0 start))
         (when (and start (1+ end) (/= start (1+ end)))
           (concatenate 'list (get-number-words (- (1+ end) start)) (list word)))
         (subseq wordlist (1+ end)))
        wordlist)))

(defun do-all-rewrites (wordlist)
  (rewrite-and
   'hundred
   following-words-symbols-only
   (rewrite-multiples
    'hundred 
    (rewrite-multiples
     'thousand
     (rewrite-and
      'thousand
      following-words-symbols-only
      wordlist)))))

(defun number-as-words-string (n)
  (apply
   #'concatenate
   (cons 'string
         (mapcar #'string
                 (do-all-rewrites (get-number-words n))))))

(defun length-as-words (n)
  (length (number-as-words-string n)))

(defun problem-17 (n)
  (reduce #'+ (loop for i from 1 to n collecting
                   (length-as-words i))))
