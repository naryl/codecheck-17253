
(in-package cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage codecheck-util
    (:use :cl)
    (:export #:last-char #:deletef #:range)))

(defpackage codecheck-fw
  (:use :cl :codecheck-util)
  (:export :build-main :cli-main))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage codecheck-ai
    (:use :cl :codecheck-util)
    (:export :build-main :cli-main)))

(in-package codecheck-util)

(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))

(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments)

(declaim (inline last-char))
(defun last-char (string)
  (char string (1- (length string))))

(defun range (num)
  (loop :for i :from 0 :below num
     :collect i))
