#!/usr/local/bin/sbcl --script

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(prompt-read "Enter text")

; prompts user yes/no and returns a boolean
(y-or-n-p "You like?")

; with-open-file handles errors and closing file
(with-open-file (out "out"
                     :direction :output
                     :if-exists :supersede)
  ; makes sure that object is formatted so taht it can be read back in
  (with-standard-io-syntax
    (print '(:a 1 :b 2 :c 3) out)))

(defvar *data* nil)

(with-open-file (in "out")
  (with-standard-io-syntax 
    (setf *data* (read in))))

(print (first *data*))

