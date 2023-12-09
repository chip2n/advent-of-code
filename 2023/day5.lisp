(in-package #:advent.day5)

;; * Part A

(defun read-until (stream predicate)
  (loop for c = (peek-char nil stream nil)
        while (and c (not (funcall predicate c)))
        collect (read-char stream nil) into chars
        finally (return (coerce chars 'string))))

(defun read-seeds (stream)
  (read-until stream (lambda (c) (char= c #\Colon)))
  (read-char stream)
  (->> (read-until stream (lambda (c) (char= c #\Newline)))
       (str:split " ")
       (mapcar #'parse-integer)))

(defun parse-map-name (name)
  (->> name
       (str:replace-first " map" "")
       (str:replace-first "-to-" "-")
       (str:split "-")
       (mapcar #'str:upcase)
       (mapcar #'a:make-keyword)))

(defun read-map (stream)
  (let ((name (->> (read-until stream (lambda (c) (char= c #\Colon)))
                   (str:trim)))
        (nums (progn (read-line stream nil)
                     (loop for line = (read-line stream nil "")
                           while (not (str:emptyp (str:trim line)))
                           collect (mapcar #'parse-integer (str:split " " line))))))
    (when (not (str:blankp name))
      (cons (parse-map-name name) nums))))

(defun read-almanac (stream)
  (let ((str:*omit-nulls* t))
    (let ((seeds (read-seeds stream))
          (maps (loop for map = (read-map stream)
                      while map
                      collect map)))
      (list seeds maps))))

(defun make-conversion (ranges)
  (flet ((make-single-conversion (range)
           (destructuring-bind (dst-start src-start len) range
             (lambda (n)
               (if (and (>= n src-start) (< n (+ src-start len)))
                   (+ n (- dst-start src-start))
                   nil)))))
    (let ((conversions (mapcar #'make-single-conversion ranges)))
      (lambda (n)
        (or
         (loop for c in conversions
               for result = (funcall c n)
               when result
                 return result)
         n)))))

(defun seed->location (conversions seed)
  (let ((i seed))
    (car (last (loop for (type . fn) in conversions
                     for next = (funcall fn i)
                     do (setf i next)
                     collect next)))))

(defun solve-a (stream)
  (let ((almanac (read-almanac stream))
        (conversions nil))
    (loop for (type . ranges) in (cadr almanac)
          do (push (cons type (make-conversion ranges)) conversions))
    (setf conversions (nreverse conversions))

    (loop for seed in (car almanac)
          minimize (seed->location conversions seed))))

;; * Tests

(defvar *sample-a* (string-trim '(#\Newline) "
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"))

(test sample-a
  (with-input-from-string (s *sample-a*)
    (is (eql 35 (solve-a s)))))
