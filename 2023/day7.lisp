(in-package #:advent.day7)

(defparameter *cards*
  '((#\A . 14)
    (#\K . 13)
    (#\Q . 12)
    (#\J . 11)
    (#\T . 10)
    (#\9 . 9)
    (#\8 . 8)
    (#\7 . 7)
    (#\6 . 6)
    (#\5 . 5)
    (#\4 . 4)
    (#\3 . 3)
    (#\2 . 2)))

(defparameter *types*
  '(((5) . :five-of-a-kind)
    ((4 1) . :four-of-a-kind)
    ((3 2) . :full-house)
    ((3 1 1) . :three-of-a-kind)
    ((2 2 1) . :two-pair)
    ((2 1 1 1) . :one-pair)
    ((1 1 1 1 1) . :high-card)))

(defun read-game (stream)
  (let ((line (read-line stream nil)))
    (when line
      (let ((parts (str:split " " line)))
        (cons (car parts) (parse-integer (cadr parts)))))))

(defun parse-hand (hand)
  (mapcar (lambda (c) (cdr (assoc c *cards*)))
          (coerce hand 'list)))

(defun hand-type (hand)
  (let ((table (make-hash-table)))
    (loop for card in hand
          do (setf (gethash card table) (1+ (a:ensure-gethash card table 0))))
    (cdr (assoc
          (sort (a:hash-table-values table) #'>)
          *types*
          :test #'equal))))

(defun hand-type-strength (type)
  (- (length *types*)
     (position-if (lambda (x) (eq (cdr x) type)) *types*)
     1))

(defun hand-compare (h1 h2)
  (when (and h1 h2)
    (if (= (car h1) (car h2))
        (hand-compare (cdr h1) (cdr h2))
        (< (car h1) (car h2)))))

(defun hand< (h1 h2)
  (let ((h1-type (hand-type h1))
        (h2-type (hand-type h2)))
    (if (= (hand-type-strength h1-type)
           (hand-type-strength h2-type))
        (hand-compare h1 h2)
        (< (hand-type-strength h1-type)
           (hand-type-strength h2-type)))))

(defun solve-a (stream)
  (let* ((games (loop for game = (read-game stream)
                      while game
                      collect (cons (parse-hand (car game)) (cdr game))))
         (sorted-games (sort games (lambda (g1 g2) (hand< (car g1) (car g2))))))
    (loop for (hand . bid) in sorted-games
          for rank from 1
          sum (* bid rank))))

;; * Tests

(defvar *sample* (string-trim '(#\Newline) "
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"))

(test sample-a
  (with-input-from-string (s *sample*)
    (is (eql 6440 (solve-a s)))))
