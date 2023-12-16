(in-package #:advent.day9)

;; * Part A

(defun parse-input (stream)
  (let ((str:*omit-nulls* t))
    (loop for line = (read-line stream nil)
          while line
          collect (mapcar #'parse-integer (str:split " " line)))))

(defun gen-child-seq (s)
  (loop for part = s then (cdr part)
        while (> (length part) 1)
        collect (- (cadr part) (car part))))

(defun zero-p (n) (= n 0))

(defun predict-seq (s)
  (if (every #'zero-p s)
      0
      (let ((child-seq (gen-child-seq s)))
        (+ (car (last s)) (predict-seq child-seq)))))

(defun solve-a (stream)
  (loop for s in (parse-input stream)
        sum (predict-seq s)))

;; * Tests

(defvar *sample-a* (string-trim '(#\Newline) "
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"))

(test sample-a
  (with-input-from-string (s *sample-a*)
    (is (eql 114 (solve-a s)))))
