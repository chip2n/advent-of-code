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

(defun read-digits (schematic x y)
  (unless (and (> y 0) (digit-char-p (aref schematic x (1- y))))
    (loop for i from y
          while (< i (car (array-dimensions schematic)))
          for c = (aref schematic x i)
          while (digit-char-p c)
          collect c)))

(defun extract-numbers (schematic)
  "Extracts all part numbers along with their x,y coords and width:
Example: ((467 0 0 3) (114 5 0 3) ...)"
  (let ((result nil))
    (destructuring-bind (n m) (array-dimensions schematic)
      (loop for i from 0 below n do
        (loop for j from 0 below m do
          (a:when-let ((digits (read-digits schematic i j)))
            (push (list (parse-integer (concatenate 'string digits)) j i (length digits)) result)))))
    (nreverse result)))

(defun extract-in-rect (arr rx ry rw rh)
  "Extract all characters inside provided rectangle."
  (destructuring-bind (sh sw) (array-dimensions arr)
    (let ((result nil))
      (loop for y from ry below (+ ry rh) do
        (loop for x from rx below (+ rx rw) do
          (when (and (>= x 0) (< x sw)
                     (>= y 0) (< y sh))
            (push (aref arr y x) result))))
      (nreverse result))))

(defun part-number-p (schematic num)
  (destructuring-bind (n x y w) num
    (declare (ignore n))
    (->>
     (extract-in-rect schematic (1- x) (1- y) (+ w 2) 3)
     (remove-if (lambda (c) (or (digit-char-p c) (char= c #\Period)))))))

(defun solve-a (stream)
  (let* ((schematic (read-schematic stream))
         (nums (extract-numbers schematic)))
    (loop for num in nums
          when (part-number-p schematic num)
            sum (car num))))

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
