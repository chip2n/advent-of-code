(in-package #:advent.day4)

;; * Part A

(defun read-until (stream predicate)
  (loop for c = (peek-char nil stream nil)
        while (and c (not (funcall predicate c)))
        collect (read-char stream nil) into chars
        finally (return (coerce chars 'string))))

(defun parse-card (stream)
  (read-until stream (lambda (c) (char= c #\:)))
  (when (read-char stream nil)
    (let ((winning-part (read-until stream (lambda (c) (char= c #\|))))
          (other-part (progn (read-char stream)
                             (read-line stream))))
      (flet ((part->nums (part)
               (let ((str:*omit-nulls* t))
                 (->> part (str:trim) (str:split " ") (mapcar #'parse-integer)))))
        (list
         (part->nums winning-part)
         (part->nums other-part))))))

(defun card-points (card)
  (let ((points 0))
    (loop for num in (cadr card)
          when (find num (car card))
            do (setf points (if (= points 0) 1 (* points 2))))
    points))

(defun solve-a (stream)
  (loop for card = (parse-card stream)
        while card
        sum (card-points card)))

;; * Tests

(defvar *sample-a* (string-trim '(#\Newline) "
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"))

(test sample-a
  (with-input-from-string (s *sample-a*)
    (is (eql 13 (solve-a s)))))
