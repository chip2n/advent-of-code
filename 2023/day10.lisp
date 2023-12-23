(in-package #:advent.day10)

;; * Part A

(defvar *pipes* nil)
(defvar *start* nil)

(defun parse-pipe (c)
  (ecase c
    (#\| :vert)
    (#\- :hori)
    (#\L :ne)
    (#\J :nw)
    (#\7 :sw)
    (#\F :se)
    (#\. :ground)
    (#\S :start)))

(defun parse-input (stream)
  (let ((pipes (loop for chars = (coerce (read-line stream nil) 'list)
                     while chars
                     collect (mapcar #'parse-pipe chars))))
    (make-array (list (length (car pipes)) (length pipes)) :initial-contents pipes :element-type 'keyword)))

(defun connects-p (pipe dir)
  (ecase dir
    (:north (member pipe '(:vert :se :sw)))
    (:south (member pipe '(:vert :ne :nw)))
    (:east (member pipe '(:hori :sw :nw)))
    (:west (member pipe '(:hori :se :ne)))))

(defun pipe-at (x y)
  (destructuring-bind (h w) (array-dimensions *pipes*)
    (unless (or (< x 0) (< y 0)
                (>= x w) (>= y h))
      (aref *pipes* y x))))

(defun pipe-dirs (pipe)
  (ecase pipe
    (:vert '(:north :south))
    (:hori '(:west :east))
    (:ne '(:north :east))
    (:nw '(:north :west))
    (:sw '(:south :west))
    (:se '(:south :east))))

(defun start-pipe (x y)
  "Get the underlying pipe for given start coordinate."
  (assert (eq (pipe-at x y) :start))
  (let ((north (pipe-at x (1- y)))
        (south (pipe-at x (1+ y)))
        (west (pipe-at (1- x) y))
        (east (pipe-at (1+ x) y)))
    (cond
      ((and (connects-p north :south) (connects-p south :north)) :vert)
      ((and (connects-p west :east) (connects-p east :west)) :hori)
      ((and (connects-p north :south) (connects-p east :west)) :ne)
      ((and (connects-p north :south) (connects-p west :east)) :nw)
      ((and (connects-p south :north) (connects-p west :east)) :sw)
      ((and (connects-p south :north) (connects-p east :west)) :se)
      (t (error "No pipe fits the start node")))))

(defun find-start ()
  (destructuring-bind (h w) (array-dimensions *pipes*)
    (loop for y from 0 below h do
      (loop for x from 0 below w
            when (eq (pipe-at x y) :start)
              do (return-from find-start (cons x y))))))

(defun load-pipes (stream)
  (setf *pipes* (parse-input stream))
  (setf *start* (find-start))
  (let* ((start-pipe (start-pipe (car *start*) (cdr *start*))))
    (setf (aref *pipes* (cdr *start*) (car *start*)) start-pipe)))

(defparameter *north* (cons 0 -1))
(defparameter *east* (cons 1 0))
(defparameter *south* (cons 0 1))
(defparameter *west* (cons -1 0))

(defvar *last-dir* nil)

;; (defun pipe-output-deltas (x y)
;;   (case (pipe-at x y)
;;     (:vert (list *north* *south*))
;;     (:hori (list *west* *east*))
;;     (:ne (list *north* *east*))
;;     (:nw (list *north* *west*))
;;     (:sw (list *south* *west*))
;;     (:se (list *south* *east*))
;;     (:start
;;      ;; (remove-if-not #'identity
;;      ;;                (list
;;      ;;                 (pipe-output-deltas x (1- y))
;;      ;;                 (pipe-output-deltas x (1+ y))
;;      ;;                 (pipe-output-deltas (1- x) y)
;;      ;;                 (pipe-output-deltas (1+ x) y)))
;;      (flet ((mirror (output)
;;               (when output
;;                 (cons (- (car output)) (- (cdr output))))))
;;        (list
;;         (mirror (pipe-output-deltas x (1- y)))
;;         (mirror (pipe-output-deltas x (1+ y)))
;;         (mirror (pipe-output-deltas (1- x) y))
;;         (mirror (pipe-output-deltas (1+ x) y))))
;;      )))

(defun step-dir (x y dir)
  (let ((pipe (pipe-at x y)))
    deltas
    )
  )

(defun make-distance-map ()
  (let* ((result (make-array (array-dimensions *pipes*)))
         (pos (cons (car *start*) (cdr *start*)))
         (dirs (pipe-dirs (pipe-at (car pos) (cdr pos)))))
    (loop do
      (step-dir (car pos) (cdr pos) (car dir))
      (step-dir (car pos) (cdr pos) (cadr dir)))
    result
    ;; (destructuring-bind (h w) (array-dimensions pipes)
    ;;   )
    ;; (values start
    ;;         start-deltas
    ;;         result)
    ))

(defun solve-a (stream)
  (load-pipes stream)
  )

;; * Tests

(defvar *sample-1* (string-trim '(#\Newline) "
.....
.S-7.
.|.|.
.L-J.
.....
"))

(defvar *sample-2* (string-trim '(#\Newline) "
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
"))

(defvar *sample-3* (string-trim '(#\Newline) "
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
"))

(defvar *sample-4* (string-trim '(#\Newline) "
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
"))

(test sample-a
  (with-input-from-string (s *sample-1*)
    (is (eql 4 (solve-a s))))
  (with-input-from-string (s *sample-2*)
    (is (eql 4 (solve-a s))))
  (with-input-from-string (s *sample-3*)
    (is (eql 8 (solve-a s))))
  (with-input-from-string (s *sample-4*)
    (is (eql 8 (solve-a s)))))
