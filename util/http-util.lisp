(load "~/quicklisp/setup.lisp")
(require :trivial-http)
(defpackage http
  (:use :common-lisp 
		:trivial-http)
  (:export :get-from-url
		   :get-code
		   :get-header
		   :get-stream
		   :read-stream))
(in-package http)

(defun get-from-url (url)
  (trivial-http:http-get url))

(defun get-code (lst)
  (car lst))

(defun get-header (lst)
  (cadr lst))

(defun get-stream (lst)
  (caddr lst))

(defun read-stream (stream)
  (handler-case 
	(let ((result ""))
	  (loop
		for line = (read-line stream nil)
		while line
		do (setf result (concatenate 'string result line)))
	  result)
	(sb-int:stream-decoding-error (c) (format t "can not decode ~a~%" c) nil)))

