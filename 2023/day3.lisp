(in-package #:advent.day3)

;; * Part A

(defun read-schematic (stream)
  (let ((contents '()))
    (push nil contents)
    (loop for c = (read-char stream nil)
          while c
          when (not (char= c #\Newline))
            do (push c (car contents))
          else
            do (setf (car contents) (reverse (car contents)))
               (push nil contents)
          finally
             (setf (car contents) (reverse (car contents)))
             (setf contents (remove-if-not #'identity (nreverse contents))))
    (make-array (list (length contents) (length (car contents)))
                :element-type 'character
                :initial-contents contents)))

(defun schematic-width (scm)
  (cadr (array-dimensions scm)))

(defun schematic-height (scm)
  (car (array-dimensions scm)))

(defun schematic-get (scm x y)
  (let ((w (schematic-width scm))
        (h (schematic-height scm)))
    (unless (or (< x 0) (>= x w)
                (< y 0) (>= y h))
      (aref scm y x))))

(defun part-numbers-around (scm x y)
  (let ((nums nil))
    (flet ((parse-mid (mid-x)
             (push (read-digits scm mid-x y) nums))
           (parse-row (row-y)
             (if (digit-at-p scm x row-y)
                 (push (read-digits scm x row-y) nums)
                 (progn
                   (push (read-digits scm (1- x) row-y) nums)
                   (push (read-digits scm (1+ x) row-y) nums)))))
      (parse-row (1- y))
      (parse-mid (1- x))
      (parse-mid (1+ x))
      (parse-row (1+ y))
      (->> (nreverse nums)
           (remove-if-not #'identity)
           (mapcar #'digits->int)))))

(defun extract-symbol-chars (scm)
  (let ((w (schematic-width scm))
        (h (schematic-height scm))
        (result nil))
    (loop for y from 0 below h do
      (loop for x from 0 below w do
        (let ((c (schematic-get scm x y)))
          (unless (or (digit-char-p c) (char= c #\Period))
            (push (list c x y) result)))))
    (nreverse result)))

(defun read-digits (scm x y)
  (unless (not (digit-at-p scm x y))
    (let ((start x))
      (loop for c = (schematic-get scm start y)
            for prev = (schematic-get scm (1- start) y)
            while (and (> start 0) (digit-char-p prev))
            do (decf start))
      (loop for i from start
            for c = (schematic-get scm i y)
            while (and c (digit-char-p c))
            collect c))))

(defun digits->int (digits)
  (parse-integer (concatenate 'string digits)))

(defun solve-a (stream)
  (let* ((scm (read-schematic stream))
         (syms (extract-symbol-chars scm)))
    (loop for (c x y) in syms
          for parts = (part-numbers-around scm x y)
          sum (reduce #'+ parts))))

;; * Part B

(defun digit-at-p (scm x y)
  (let ((c (schematic-get scm x y)))
    (and c (digit-char-p c))))

(defun extract-gears (scm)
  (->> (extract-symbol-chars scm)
       (remove-if-not (lambda (s) (char= (car s) #\Asterisk)))))

(defun solve-b (stream)
  (let* ((scm (read-schematic stream))
         (gears (extract-gears scm)))
    (loop for (_ x y) in gears
          for parts = (part-numbers-around scm x y)
          when (= (length parts) 2)
            sum (reduce #'* parts))))

;; * Tests

(defvar *sample-a* (string-trim '(#\Newline) "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"))

(test sample-a
  (with-input-from-string (s *sample-a*)
    (is (eql 4361 (solve-a s)))))

(defvar *sample-b* (string-trim '(#\Newline) "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"))

(test sample-b
  (with-input-from-string (s *sample-b*)
    (is (eql 467835 (solve-b s)))))
