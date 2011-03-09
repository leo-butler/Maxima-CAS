;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline create-scanner parse-string))
(defun parse-string (string)
  (pregexp string))
(defun create-scanner (&rest args)
  (apply #'compile-regex args))
(defmacro match-begin (match)
  `(caar ,match))
(defmacro match-end (match)
  `(cdar ,match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pregexp scan and scan-to-strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :pregexp)

(defun pregexp-scan (re str &key (start 0) (end (length str)))
  (let* ((matches (pregexp-match-positions re str start end))
	 (b (match-begin matches))
	 (e (match-end matches))
	 (l (if matches (- (length matches) 1) 0))
	 (mb (make-array l :element-type 'fixnum))
	 (me (make-array l :element-type 'fixnum)))
    (do ((i 0             (1+ i))
	 (m (cdr matches) (cdr m)))
	((null m)
	 (values b e mb me))
      (setf (aref mb i) (match-begin m) (aref me i) (match-end m)))))

(defun pregexp-scan-to-strings (re str &key (start 0) (end (length str)))
  (let* ((matches (pregexp-match re str start end))
	 (b (car matches))
	 (l (if matches (- (length matches) 1) 0))
	 (ms (make-array l :element-type 'string :initial-element "")))
    (do ((i 0             (1+ i))
	 (m (cdr matches) (cdr m)))
	((null m)
	 (values b ms))
      (setf (aref ms i) (car m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl-ppcre api layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *scan*            (symbol-function 'pregexp-scan))
(defvar *scan-to-strings* (symbol-function 'pregexp-scan-to-strings))
(defvar *create-scanner*  (symbol-function 'compile-regex))
(defvar *default-register-number* 10)

;; end of cl-ppcre-interface.lisp 
