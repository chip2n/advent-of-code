(in-package #:advent)

;; * Day 1a

(defun parse-calibration-value (line)
  (let ((digits nil))
    (loop for c across line
          when (digit-char-p c)
            do (push c digits)
               (push c digits))
    (setf digits (nreverse digits))
    (when digits
      (parse-integer
       (concatenate 'string (list (first digits) (car (last digits))))))))

(defun solve-day1a (stream)
  (loop for line = (read-line stream nil)
        while line
        summing (parse-calibration-value line)))

(defvar *day1a-sample* (string-trim '(#\Newline) "
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"))

(test day1a-sample
  (with-input-from-string (s *day1a-sample*)
    (is (eql 142 (solve-day1a s)))))

;; * Day 1b

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *digits*
    '((1 . "one")
      (2 . "two")
      (3 . "three")
      (4 . "four")
      (5 . "five")
      (6 . "six")
      (7 . "seven")
      (8 . "eight")
      (9 . "nine")))

  (defun string-chars (s)
    (loop for c across s collect c))

  (defun build-graph-single-path (chars)
    (if chars
        (list (cons (car chars) (build-graph-single-path (cdr chars))))
        t))

  (defun merge-graph-paths (p1 p2)
    (if p1
        (if (char= (caar p1) (caar p2))
            (cons
             (cons (caar p1) (merge-graph-paths (cdar p1) (cdar p2)))
             (cdr p1))
            (cons (car p1) (merge-graph-paths (cdr p1) p2)))
        p2)))

(defmacro compile-digit-graph ()
  `'(,@(loop for (n . s) in *digits*
             collect (cons (digit-char n) t))
     ,@(reduce #'merge-graph-paths
        (mapcar (lambda (d) (build-graph-single-path (string-chars (cdr d)))) *digits*))))

(defparameter *digit-graph*
  (compile-digit-graph))

(defun read-next-digit (chars)
  (let* ((c (car chars))
         (graph (cdr (assoc c *digit-graph*))))
    (when (and c graph)
      (if (eq graph t)
          (list c)
          (let ((next (let ((*digit-graph* graph))
                        (read-next-digit (cdr chars)))))
            (if next
                (cons c next)
                (read-next-digit (cdr chars))))))))

(defun read-digits (chars)
  (when chars
    (a:if-let ((digit (read-next-digit chars)))
      (cons digit (read-digits (cdr chars)))
      (read-digits (cdr chars)))))

(defun parse-digit (s)
  (or (car (rassoc s *digits* :test 'string=))
      (parse-integer s)))

(defun make-calibration-value (digits)
  (parse-integer
   (concatenate 'string
                (write-to-string (first digits))
                (write-to-string (car (last digits))))))

(defun solve-day1b (stream)
  (loop for line = (read-line stream nil)
        while line
        for res = (read-digits (string-chars line))
        summing (make-calibration-value
                 (mapcar (lambda (x) (parse-digit (concatenate 'string x))) res))))

(defvar *day1b-sample* (string-trim '(#\Newline) "
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"))

(test day1b-sample
  (with-input-from-string (s *day1b-sample*)
    (is (eql 281 (solve-day1b s)))))

;; * Day 2a

(defun read-until (stream predicate)
  (loop for c = (peek-char nil stream nil)
        while (and c (not (funcall predicate c)))
        collect (read-char stream nil) into chars
        finally (return (coerce chars 'string))))

(defun read-until-char (stream c)
  (read-until stream (lambda (x) (char= c x))))

(defun read-char-n (stream n)
  (loop for i from 0 below n
        for c = (read-char stream nil)
        while c
        collect c))

(defun read-integer (stream)
  (parse-integer
   (read-until stream (lambda (c) (not (digit-char-p c))))))

(defun drop (n s)
  (subseq s (min n (length s))))

(defun parse-round (s)
  (->> (str:trim s)
       (str:split ", ")
       (mapcar #'parse-hand)))

(defun parse-hand (s)
  (let ((res (str:split " " s)))
    (cons (parse-integer (car res))
          (a:make-keyword (str:upcase (cadr res))))))

(defparameter *legal-games*
  '((12 . :red)
    (13 . :green)
    (14 . :blue)))

(defun parse-dice-game (line)
  (with-input-from-string (stream line)
    (let ((game-id (parse-integer (drop 5 (read-until-char stream #\Colon))))
          (rounds (->> (progn (read-char-n stream 2) (read-line stream))
                       (str:split ";")
                       (mapcar #'parse-round))))
      (cons game-id rounds))))

#+(or)
(progn
  (parse-dice-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))

(defun all (seq)
  (every #'identity seq))

(defun hand-legal-p (hand)
  (<= (car hand)
      (car (rassoc (cdr hand) *legal-games*))))

(defun round-legal-p (round)
  (all (mapcar #'hand-legal-p round)))

(defun solve-day2a (stream)
  (loop for line = (read-line stream nil)
        while line
        for (id . rounds) = (parse-dice-game line)
        when (all (mapcar #'round-legal-p rounds))
          summing id))

(defvar *day2a-sample* (string-trim '(#\Newline) "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"))

(test day2a-sample
  (with-input-from-string (s *day2a-sample*)
    (is (eql 8 (solve-day2a s)))))

;; * Main

(defun solve (solver input-path)
  (with-open-file (s input-path)
    (funcall solver s)))
