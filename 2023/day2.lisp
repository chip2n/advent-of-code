(in-package #:advent.day2)

;; * Part A

(defparameter *legal-games*
  '((12 . :red)
    (13 . :green)
    (14 . :blue)))

(defun all (seq)
  (every #'identity seq))

(defun drop (n s)
  (subseq s (min n (length s))))

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

(defun parse-round (s)
  (->> (str:trim s)
       (str:split ", ")
       (mapcar #'parse-hand)))

(defun parse-hand (s)
  (let ((res (str:split " " s)))
    (cons (parse-integer (car res))
          (a:make-keyword (str:upcase (cadr res))))))

(defun parse-dice-game (line)
  (with-input-from-string (stream line)
    (let ((game-id (parse-integer (drop 5 (read-until-char stream #\Colon))))
          (rounds (->> (progn (read-char-n stream 2) (read-line stream))
                       (str:split ";")
                       (mapcar #'parse-round))))
      (cons game-id rounds))))

(defun hand-legal-p (hand)
  (<= (car hand)
      (car (rassoc (cdr hand) *legal-games*))))

(defun round-legal-p (round)
  (all (mapcar #'hand-legal-p round)))

(defun solve-a (stream)
  (loop for line = (read-line stream nil)
        while line
        for (id . rounds) = (parse-dice-game line)
        when (all (mapcar #'round-legal-p rounds))
          summing id))

(defvar *sample-a* (string-trim '(#\Newline) "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"))

(test sample-a
  (with-input-from-string (s *sample-a*)
    (is (eql 8 (solve-a s)))))

;; * Part B

(defun hand-power (hand)
  (reduce #'* (mapcar #'car hand)))

(defun min-round (round)
  (let ((table (make-hash-table)))
    (loop for (n . color) in (apply #'concatenate 'list round)
          for curr = (gethash color table 0)
          do (setf (gethash color table) (max curr n)))
    (loop for color being the hash-key
            using (hash-value n) of table
          collect (cons n color))))

(defun solve-b (stream)
  (loop for line = (read-line stream nil)
        while line
        for (id . rounds) = (parse-dice-game line)
        summing (hand-power (min-round rounds))))

(defvar *sample-b* (string-trim '(#\Newline) "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"))

(test sample-b
  (with-input-from-string (s *sample-b*)
    (is (eql 2286 (solve-b s)))))
