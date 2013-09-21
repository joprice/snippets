#!/usr/local/bin/sbcl --script

(defpackage :com.test.pack
  (:use :common-lisp))

(in-package :com.test.pack)

(defclass person ()
           ((name
              :initarg :name
              :accessor name
              :initform (error "Must provide :name")
              :documentation "Name of person.")
            (age
              :initarg :age
              :accessor age
              :initform 0
              )))

(defmethod test ((p person))
  (name p))

(let ((p (make-instance 'person :name "John" :age 40)))
  (test p))


