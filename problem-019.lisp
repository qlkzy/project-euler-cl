;; Solution to Project Euler Problem 19

;; Counting the number of Sundays that fell on the first of the month
;; during the 20th century.

(defparameter day-month-alist
  '((1  . 31)
    (2  . 28)
    (3  . 31)
    (4  . 30)
    (5  . 31)
    (6  . 30)
    (7  . 31)
    (8  . 31)
    (9  . 30)
    (10 . 31)
    (11 . 30)
    (12 . 31)))

(defun leap-year? (year)
  "Return true if YEAR is the number of a leap year."
  (and (= 0 (mod year 4))
       (or (/= 0 (mod year 100))
           (= 0 (mod year 400)))))

(defun days-in-year (year)
  (if (leap-year? year) 366 365))

(defun days-in-years (years)
  (reduce #'+ (mapcar #'days-in-year years)))

(defun days-upto-year (year)
  (days-in-years
   (loop for y from 1900 upto (- year 1) collect y)))

(defun days-in-month (month year)
  (if (and (= month 2) (leap-year? year))
      29
      (cdr (assoc month day-month-alist))))

(defun days-in-months (months year)
  (reduce #'+ (mapcar (lambda (m) (days-in-month m year)) months)))

(defun days-upto-month (month year)
  (+ (days-upto-year year)
     (days-in-months (loop for m from 1 upto (- month 1) collect m) year)))

(defun days-upto-date (day month year)
  (+ day
     (days-upto-month month year)))

(defun date-to-day-of-week (day month year)
  (mod (days-upto-date day month year) 7))

(defun years-upto-day (day)
  (do ((year 1900 (+ year 1))
       (theday day (- theday (days-in-year year))))
      ((> (days-in-year year) theday) (values theday year))))

(defun day-to-date (day)
  (multiple-value-bind (days year) (years-upto-day day)
    (do ((month 1 (+ month 1))
         (theday days (- theday (days-in-month month year))))
        ((> (days-in-month month year) theday) (values theday month year)))))

(defun first-of-month? (day)
  (= (day-to-date day) 1))

(defun sunday? (day)
  "Return true if DAY (number of days since 1 Jan 1900) is a Sunday."
  (= 0 (mod day 7)))

(defun problem-19 ()
  (length
   (loop for d
      from (days-upto-date 1 1 1901)
      to (days-upto-date 31 12 2000)
      when (and (sunday? d)
                (first-of-month? d))
      collect d)))

;; (defun year (day)
  ;; "Return the year for DAY (number of days since 1 Jan 1990)."

