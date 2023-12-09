(in-package #:advent.day6)

;; * Part A

(defun read-until (stream predicate)
  (loop for c = (peek-char nil stream nil)
        while (and c (not (funcall predicate c)))
        collect (read-char stream nil) into chars
        finally (return (coerce chars 'string))))

(defun read-numbers (stream)
  (read-until stream (lambda (c) (char= c #\Colon)))
  (read-char stream)
  (->> (read-line stream)
       (str:split " ")
       (mapcar #'parse-integer)))

(defun enumerate-options (duration)
  (loop for i from 0 upto duration
        for distance = (* i (- duration i))
        collect distance))

(defun solve-a (stream)
  (let ((str:*omit-nulls* t))
    (let ((durations (read-numbers stream))
          (distances (read-numbers stream)))
      (reduce #'*
              (loop for duration in durations
                    for distance in distances
                    for options = (enumerate-options duration)
                    collect (length (remove-if (lambda (d) (>= distance d)) options)))))))

;; * Part B

(defun read-numbers-kerning (stream)
  (read-until stream (lambda (c) (char= c #\Colon)))
  (read-char stream)
  (->> (read-line stream)
       (str:split " ")
       (apply #'concatenate 'string)
       (parse-integer)))

(defun count-options (duration record)
  (let* ((min (loop for i from 0 upto duration
                    for distance = (* i (- duration i))
                    when (< record distance)
                      return i))
         (max (loop for i from min upto duration
                    for distance = (* i (- duration i))
                    when (> record distance)
                      return (1- i))))
    (if (and min max)
        (1+ (- max min))
        0)))

(defun solve-b (stream)
  (let ((str:*omit-nulls* t))
    (let* ((duration (read-numbers-kerning stream))
           (record (read-numbers-kerning stream)))
      (count-options duration record))))

;; * Tests

(defvar *sample* (string-trim '(#\Newline) "
Time:      7  15   30
Distance:  9  40  200
"))

(test sample-a
  (with-input-from-string (s *sample*)
    (is (eql 288 (solve-a s)))))

(test sample-b
  (with-input-from-string (s *sample*)
    (is (eql 71503 (solve-b s)))))
