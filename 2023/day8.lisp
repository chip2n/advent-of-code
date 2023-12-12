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

;; * Part B

(defvar *next-end-nodes* nil)

(defun read-directions-array (stream)
  (let* ((line (read-line stream nil))
         (dirs (mapcar (lambda (c) (if (char= c #\R) :right :left))
                       (coerce line 'list))))
    (make-array (length dirs) :element-type 'keyword :initial-contents dirs)))

(defstruct node
  (id 0 :type fixnum)
  (left 0 :type fixnum)
  (right 0 :type fixnum)
  (start-p nil :type boolean)
  (end-p nil :type boolean))

(defun read-network-array (stream)
  (let ((defs (loop for line = (read-line stream nil)
                    while line
                    for (node edges-str) = (str:split " = " line)
                    for edges = (->> edges-str
                                     (str:replace-all "(" "")
                                     (str:replace-all ")" "")
                                     (str:split ", "))
                    unless (str:blankp line)
                      collect (cons node edges)))
        (id-table (make-hash-table :test 'equal)))
    ;; Replace node names with an integer for easier storage in arrays
    (loop for node in (mapcar #'car defs)
          for i from 0
          do (setf (gethash node id-table) i))
    ;; Use the new IDs to build the network
    (let ((network (make-array (length defs) :element-type 'node :initial-element (make-node))))
      (loop for def in defs
            for node = (gethash (car def) id-table)
            for left = (gethash (cadr def) id-table)
            for right = (gethash (caddr def) id-table)
            do (setf (aref network node)
                     (make-node :id node :left left :right right
                                :start-p (string-equal (str:s-last (car def)) "A")
                                :end-p (string-equal (str:s-last (car def)) "Z"))))
      network)))

(defun find-next-end-node (network dirs node &optional (dir-offset 0) (step-count 0))
  (let* ((left (aref network (node-left node)))
         (right (aref network (node-right node))))
    (let ((dir (aref dirs dir-offset)))
      (ecase dir
        (:left
         (if (node-end-p left)
             (cons (node-id left) (1+ step-count))
             (find-next-end-node network dirs left (mod (1+ dir-offset) (length dirs)) (1+ step-count))))
        (:right
         (if (node-end-p right)
             (cons (node-id right) (1+  step-count))
             (find-next-end-node network dirs right (mod (1+ dir-offset) (length dirs)) (1+ step-count))))))))

(defun cached-next-node (node dir-offset)
  (let ((entry (aref *next-end-nodes* (node-id node) dir-offset)))
    (unless (= (car entry) -1) entry)))

(defun next-node (network dirs node dir-offset)
  (let ((dir-offset (mod dir-offset (length dirs))))
    (or (cached-next-node node dir-offset)
        (setf (aref *next-end-nodes* (node-id node) dir-offset)
              (find-next-end-node network dirs node dir-offset)))))

(defun build-next-array (network dirs)
  (let* ((result (make-array (list (length network) (length dirs))
                             :element-type '(cons fixnum integer)
                             :initial-element (cons -1 -1))))
    (loop for node across network
          when (node-start-p node)
            do (setf (aref result (node-id node) 0)
                     (find-next-end-node network dirs node)))
    result))

(defun step-nodes (nodes steps i step-count)
  (let ((node (aref nodes i))
        (n (aref steps i)))
    (if (and (> i 0) (= step-count n))
        (1+ i)
        (loop named loop
              for (next . m) = (next-node *network* *dirs* node n)
              do (setf (aref nodes i) (aref *network* next))
                 (incf (aref steps i) m)

                 (if (= (aref steps i) step-count)
                     (return-from loop (1+ i)))

                 (if (> (aref steps i) step-count)
                     (return-from loop (if (= i 0) 1 0)))))))

(defun solve-b (stream)
  (let* ((dirs (read-directions-array stream))
         (network (read-network-array stream)))
    (setf *network* network)
    (setf *dirs* dirs)
    (setf *next-end-nodes* (build-next-array network dirs))

    (let* ((start-nodes (loop for node across *network*
                              when (node-start-p node)
                                collect node))
           (current-nodes (make-array (length start-nodes) :initial-contents start-nodes :element-type 'node))
           (current-steps (make-array (length start-nodes) :initial-element 0 :element-type 'fixnum)))
      (setf *current-nodes* current-nodes)
      (setf *current-steps* current-steps)

      (let ((step-count 0)
            (i 0))
        (loop for new-i = (step-nodes current-nodes current-steps i step-count)
              until (= new-i (length start-nodes))
              do (setf step-count (aref current-steps i))
                 (setf i new-i))
        step-count))))

;; * Tests

(defvar *sample-a* (string-trim '(#\Newline) "
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
  (with-input-from-string (s *sample-a*)
    (is (eql 2 (solve-a s)))))

(defvar *sample-b* (string-trim '(#\Newline) "
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"))

(test sample-b
  (with-input-from-string (s *sample-b*)
    (is (eql 6 (solve-b s)))))
