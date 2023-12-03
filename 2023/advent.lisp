(in-package #:advent)

;; * Day 1

(defun calibration-digit (line)
  (let ((digits nil))
    (loop for c across line
          when (digit-char-p c)
            do (push c digits)
               (push c digits))
    (setf digits (nreverse digits))
    (when digits
      (parse-integer
       (concatenate 'string (list (first digits) (car (last digits))))))))

(defun solve-day1 (stream)
  (loop for line = (read-line stream nil)
        while line
        summing (calibration-digit line)))

(defvar *day1-sample* (string-trim '(#\Newline) "
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"))

(test day1-sample
  (with-input-from-string (s *day1-sample*)
    (is (eql 142 (solve-day1 s)))))

;; * Main

(defun solve (solver input-path)
  (with-open-file (s input-path)
    (funcall solver s)))
