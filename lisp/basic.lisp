#!/usr/local/bin/sbcl --script

; get help
(describe 'format)
(documentation 'format 'function)

; declare function 
(defun test (n)
  (print "test 1")
  (print "test 2"))

(defun noargs () 
  (print "no args"))

; call function 
(test 500)

; call anonymous function
(print (funcall (lambda (x y) (list x y y)) :a :b))

; named arguments - can have default value
; z-p is supplied-p parameter - a boolean signaling when a keyword argument was given
(defun named (&key y (z 5 z-p)) 
  (cons y z) '(y z z-p))

(named :y "a" :z "b")
(named :y "a")

; reference function itself
(function +)
; equivalent to
#'+

; calls function with supplied args
(funcall #'+ 1 2)
; last argument should be list
(apply #'+ 1 2 '(3 4))
; returns another list of one-argument function called on each element of supplied list
(mapcar #'not '(t nil t nil))

(remove-if #'evenp '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))

(delete-duplicates '(1 2 2 2 3 3 4 4))

; disable an expression with #+nil
#+nil
(print 1)

;; variadic arguments
(defun variadic (a &rest others)
  (cons a others))

; can be called with any number of args
(print (variadic 1 2 3 4 5))
; also with none
(print (variadic 2))

;; optional arguments
; if optional param is not provided, it is nil
(defun optional-arg (a &optional optional)
  (list a optional))

; define auxilary argument based on other argument
(funcall (lambda (x &aux (y (+ 1 x)))
           ; t for standard out
           (format t "x: ~a~%" x y)) 5)

; returns (2 3)
(print (optional-arg 2 3))
; returns (2 NIL)
(print (optional-arg 2))

; iteration
(dolist (item '(1 2 3))
  (format t "~a~%" item))

(dotimes (i 3)
  (format t "~a~%" i))

; loop macro - see  http://www.gigamonkeys.com/book/loop-for-black-belts.html
(loop for i from 1 to 10 do (print i))

; loop and stop whichever is lower - list length or count
(loop for item in '(1 2 3) for i from 1 to 10 do (print i))

(do ((x 1 (+ x 1))
     (y 1 (* y 2)))
  ((> x 5) y)
  (print y)
  (print 'working))

; run multiple commands - returns value of last
(progn 
  (print 1) 
  (print 2))

; access command line arguments
(if (> (length sb-ext:*posix-argv*) 1)
  (print (nth 2 sb-ext:*posix-argv*))
  (print "Not enough arguments"))

; define regular variable
(set 'regular 50)
; define variable that will be reset when script is reloaded
(defparameter *b* 50)
; define special variable
(defvar *a* 10)

; mutate variable
(setq *a* 20)

; declare local variable
(let ((local "a")) (print local)) 

; temporarily bind a to another value
(let ((a 11)) (print a))

; t is true
(print (if t 2 4))
; (not t) is nil
(print (if (not t) 2 4))

; keyword "self-evaluating symbol"
:q

; complex numbers #c(real imaginary)
#c(1.2e-10 0.75)

; rational number
1/2

; e
(exp 1)

; a^b
(expt 2 4)

; first element of cons
(car (cons 1 2))

; second element of cons
(cdr (cons 1 2))

; list
(list 1 2 3)


; stack
(defvar s nil)
(push 10 s)
(push 50 s)
(pop s)

; plist - poor man's hash table
(getf (list :a 1 :b 2) :a)

(parse-integer "2")
; returns nil on error
(parse-integer "t" :junk-allowed t)

(make-array 6)
; dimensions
(make-array '(1 2 3))
; aref accesses elements
(aref (make-array '(2 2)) 1 0)

; create record - defines make-foo, client-name, client-phone client-address
(defstruct client name phone address)

(defvar ted (make-client 
              :name "Ted"
              :phone 28248248
              :address "abcd"))
(client-name ted)
; update field in record
(setf (client-phone ted) 23434242)

; named block 
(block b (return-from b 10) 50)

; numerical equality
(= 1 1)
; reference equality
(eql 'a 'a)
; value equality
(equal '(1 2 3) '(1 2 3))

; backtick is like quote, but allows a comma to be used to escape quoting
`(1 2 ,(+ 1 2))
; use @ to splice list into surrounding list
`(1 2 ,@(list 3 4) 5)

(defvar *map* (make-hash-table :test #'equal))


(case 1 (1 "a") (2 "b"))
; exhaustive case - error if no match
(ecase 1 (1 "a") (2 "b"))


