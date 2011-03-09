;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pregexp scan and scan-to-strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :maxima-nregex)

(defun nregex-scan (re str &key (start 0) (end (length str)) (sharedp t))
  (declare (ignorable sharedp))
  (nregex-flet-1 regex-match re
		 (let (regex-groups regex-groupings f-match)
		   (cond ((multiple-value-setq (regex-groups regex-groupings) (regex-match str start end))
			  (info "~a~%~a~%" regex-groups regex-groupings)
			  (let* ((l (1- regex-groupings))
				 (b-registers (make-array l :element-type 'fixnum))
				 (e-registers (make-array l :element-type 'fixnum)))
			    (loop for i from 0 to (1- l)
			       for register = (aref regex-groups (1+ i))
			       for b = (car register)
			       for e = (cadr register)
			       do (setf (aref b-registers i) b
					(aref e-registers i) e))
			    (setq f-match (aref regex-groups 0))
			    (values (car f-match) (cadr f-match) b-registers e-registers)))
			 (t nil)))))

(defun nregex-scan-to-strings (re str &key (start 0) (end (length str)) (sharedp t))
  (declare (ignorable sharedp))
  (nregex-flet-strings-1 regex-match re
			 (let (match regex-groups regex-groupings)
			   (cond ((multiple-value-setq (match regex-groups regex-groupings) (regex-match str start end))
				  (let ((registers (make-array (1- regex-groupings) :element-type 'string :initial-element "")))
				    (loop for r in (cdr match)
				       for j = 0 then (1+ j)
				       do (setf (aref registers j) r))
				    (values (car match) registers)))
				 (t nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline create-scanner parse-string))
(defun parse-string (string)
  (regex-compile string))
(defun create-scanner (&rest args)
  (apply #'compile-regex args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl-ppcre api layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *scan*            (symbol-function 'nregex-scan))
(defvar *scan-to-strings* (symbol-function 'nregex-scan-to-strings))
(defvar *create-scanner*  (symbol-function 'compile-regex))
(defvar *default-register-number* 10)

;; end of cl-ppcre-interface.lisp 
