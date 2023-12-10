(in-package #:advent.day8)

;; * Part A

(defvar *network* nil)

(defun read-until (stream predicate)
  (loop for c = (peek-char nil stream nil)
        while (and c (not (funcall predicate c)))
        collect (read-char stream nil) into chars
        finally (return (coerce chars 'string))))

(defun read-network (stream)
  (loop for line = (read-line stream nil)
        while line
        for (node edges-str) = (str:split " = " line)
        for edges = (->> edges-str
                        (str:replace-all "(" "")
                        (str:replace-all ")" "")
                        (str:split ", "))
        unless (str:blankp line)
          collect (cons node edges)))

(defun read-directions (stream)
  (let ((line (read-line stream nil)))
    (mapcar (lambda (c) (if (char= c #\R) :right :left))
            (coerce line 'list))))

(defun network-get (node)
  (cdr (assoc node *network* :test #'string-equal)))

(defun node-equal (n1 n2)
  (string-equal n1 n2))

(defun walk (node dir)
  (let ((current (network-get node)))
    (ecase dir
      (:left (car current))
      (:right (cadr current)))))

(defun circular (items)
  (setf (cdr (last items)) items)
  items)

(defun walk-to (start end dirs &optional (count 1))
  (let* ((node start)
         (dir (car dirs))
         (next (walk node dir)))
    (if (node-equal end next)
        count
        (walk-to next end (cdr dirs) (1+ count)))))

(defun solve-a (stream)
  (let* ((dirs (read-directions stream))
         (*network* (read-network stream)))
    (walk-to "AAA" "ZZZ" (circular dirs))))

;; * Tests

(defvar *sample* (string-trim '(#\Newline) "
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"))

(test sample-a
  (with-input-from-string (s *sample*)
    (is (eql 2 (solve-a s)))))
