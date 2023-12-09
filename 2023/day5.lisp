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

;; * Part B

(defun make-range (start end) (cons start end))
(defun range-start (r) (car r))
(defun range-end (r) (cdr r))

(defun between-p (n min max &key inclusive-p)
  (and (or (< n max) (and inclusive-p (= n max)))
       (>= n min)))

(defun range-contains-p (range n &key inclusive-p)
  (between-p n (range-start range) (range-end range) :inclusive-p inclusive-p))

(defun range-intersection (r1 r2)
  (let ((start (max (range-start r1) (range-start r2)))
        (end (min (range-end r1) (range-end r2))))
    (when (> (- end start) 0)
      (make-range start end))))

(defun resolve-range (seed-range map)
  (destructuring-bind (dst src len) map
    (let* ((src-range (make-range src (+ src len)))
           (intersection (range-intersection seed-range src-range)))
      (when intersection
        (list intersection (- dst src))))))

(defun resolve-ranges (seed-range maps)
  (remove-if-not #'identity
                 (mapcar (lambda (m) (resolve-range seed-range m)) maps)))

(defun range-diff (r1 r2)
  (let ((s1 (range-start r1))
        (e1 (range-end r1))
        (s2 (range-start r2))
        (e2 (range-end r2)))
    (let ((p1 (when (and (> s2 s1) (range-contains-p r1 s2)) s2))
          (p2 (when (and (< e2 e1) (range-contains-p r1 e2 :inclusive-p t)) e2)))
      (cond
        ((>= s1 e2) (list r1))
        ((<= e1 s2) (list r1))
        ((and p1 p2) (list (make-range s1 p1)
                           (make-range p2 e1)))
        (p1 (list (make-range s1 p1)))
        (p2 (list (make-range p2 e1)))))))

(defun range-diff-multi (r1 ranges)
  (if ranges
      (let ((new (range-diff r1 (car ranges))))
        (loop for result in new
              nconc (range-diff-multi result (cdr ranges))))
      (list r1)))

(defun resolve-seed-range (seed-range maps)
  (when maps
    (let ((result nil))
      (loop for (range offset) in (resolve-ranges seed-range (car maps))
            do (push (list range offset) result))
      (setf result (nreverse result))
      (let ((rest-ranges
              (if result
                  (remove-if-not #'identity
                                 (range-diff-multi seed-range
                                                   (mapcar #'car result)))
                  (list seed-range))))
        (concatenate 'list
                     result
                     (mapcar (lambda (r) (list r 0)) rest-ranges))))))

(defun seed-min-location (seed-range maps)
  (if maps
      (let ((ranges (resolve-seed-range seed-range maps)))
        (loop for (range offset) in ranges
              minimizing (seed-min-location
                          (make-range (+ (range-start range) offset)
                                      (+ (range-end range) offset))
                          (cdr maps))))
      (range-start seed-range)))

(defun chunk-seeds (seeds)
  (when seeds
    (cons
     (cons (car seeds) (cadr seeds))
     (chunk-seeds (cddr seeds)))))

(defun solve-b (stream)
  (let* ((almanac (read-almanac stream))
         (maps (mapcar #'cdr (cadr almanac))))
    (loop for (start . len) in (chunk-seeds (car almanac))
          for seed-range = (make-range start (+ start len))
          minimize (seed-min-location seed-range maps))))

;; * Tests

(defvar *sample* (string-trim '(#\Newline) "
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
  (with-input-from-string (s *sample*)
    (is (eql 35 (solve-a s)))))

(test sample-b
  (with-input-from-string (s *sample*)
    (is (eql 46 (solve-b s)))))
