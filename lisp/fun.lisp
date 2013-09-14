#!/usr/local/bin/sbcl --script

(defun test (n)
  (dotimes (i n)
    (format t "~a~%" i)))

(test 500000)

