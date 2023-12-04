(in-package #:advent)

(defun solve (solver input-path)
  (with-open-file (s input-path)
    (funcall solver s)))
