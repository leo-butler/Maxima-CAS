;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :pregexp)

(declaim (inline create-scanner parse-string))
(defun parse-string (string)
  (pregexp string))
(defun create-scanner (&rest args)
  (apply #'compile-regex args))
(defmacro match-begin (match)
  `(caar ,match))
(defmacro match-end (match)
  `(cdar ,match))
(defvar *default-register-number* 10)
(defun count-registers (regex)
  (if (stringp regex)
      (length (remove-if-not (lambda(x) (char= x #\()) (coerce regex 'list)))
      *default-register-number*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pregexp scan and scan-to-strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scan (re str &key (start 0) (end (length str)))
  (setf str (subseq str start end))
  (let* ((matches (pregexp-match-positions re str))
	 (b (if matches (+ start (match-begin matches))))
	 (e (if matches (+ start (match-end matches))))
	 (l (if matches (- (length matches) 1) 0))
	 (mb (make-array l :element-type 'fixnum))
	 (me (make-array l :element-type 'fixnum)))
    (do ((i 0             (1+ i))
	 (m (cdr matches) (cdr m)))
	((null m)
	 (values b e mb me))
      (setf (aref mb i) (+ start (match-begin m))
	    (aref me i) (+ start (match-end m))))))

(defun scan-to-strings (re str &key (start 0) (end (length str)))
  (setf str (subseq str start end))
  (let* ((matches (pregexp-match re str))
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

(defvar *scan*            (symbol-function 'scan))
(defvar *scan-to-strings* (symbol-function 'scan-to-strings))
(defvar *create-scanner*  (symbol-function 'compile-regex))

;; end of cl-ppcre-interface.lisp 
