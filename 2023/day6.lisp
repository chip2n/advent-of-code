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

;; * Tests

(defvar *sample* (string-trim '(#\Newline) "
Time:      7  15   30
Distance:  9  40  200
"))

(test sample-a
  (with-input-from-string (s *sample*)
    (is (eql 288 (solve-a s)))))
