;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :gcl-regex)

;; (declaim (inline create-scanner parse-string))
;; (defun parse-string (string)
;;   (pregexp string))
;; (defun create-scanner (&rest args)
;;   (apply #'compile-regex args))
;; (defmacro match-begin (match)
;;   `(caar ,match))
;; (defmacro match-end (match)
;;   `(cdar ,match))
;; (defvar *default-register-number* 10)
;; (defun count-registers (regex)
;;   (if (stringp regex)
;;       (length (remove-if-not (lambda(x) (char= x #\()) (coerce regex 'list)))
;;       *default-register-number*))
(defparameter *newline-as-char-list*
  '(
    #+windows
    #\Page#\Return
    #+macintosh
    #\Return
    #+unix
    #\Newline
    )
  )
(defparameter *newline* (coerce *newline-as-char-list* 'string))
(defun translate-newlines (s)
  (declare (special *newline*))
  (let ((s (if (consp s) s (coerce s 'list)))
	r)
    (labels ((t-n (str)
	       ;;(format t "~a~%" str)
	       (cond ((null str)
		      str)
		     ((and (char= (car str) #\\) (char= (cadr str) #\n))
		      (append *newline-as-char-list* (t-n (cddr str))))
		     (t
		      (cons (car str) (t-n (cdr str)))))))
      (setf r (t-n s))
      (coerce r 'string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pregexp scan and scan-to-strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scan (re str &key (start 0) (end (length str)))
  (setf re (translate-newlines re))
  (flet ((length-matches ()
	   (do ((i 0 (1+ i)))
	       ((eq -1 (system:match-beginning i))
		(1- i)))))
    (setf str (subseq str start end))
    (let ((matches (system:string-match re str)))
      (if (eq matches -1) (return-from scan nil))
      (let* ((b (+ start (system:match-beginning 0)))
	     (e (+ start (system:match-end 0)))
	     (l (length-matches))
	     (mb (make-array l :element-type 'fixnum))
	     (me (make-array l :element-type 'fixnum)))
	(dotimes (i l
		  (values b e mb me))
	  (setf (aref mb i) (+ start (system:match-beginning (1+ i)))
		(aref me i) (+ start (system:match-end (1+ i)))))))))

(defun scan-to-strings (re str &key (start 0) (end (length str)))
  (setf re (translate-newlines re))
  (let (b e mb me)
    (multiple-value-setq (b e mb me) (scan re str :start start :end end))
    (if (null b) (return-from scan-to-strings nil))
    (let* ((m (subseq str b e))
	   (l (length mb))
	   (match-strings (make-array l :element-type 'string :initial-element "")))
      (dotimes (i l
		(values m match-strings))
	(setf (aref match-strings i) (subseq str (aref mb i) (aref me i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl-ppcre api layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *scan*            (symbol-function 'scan))
(defvar *scan-to-strings* (symbol-function 'scan-to-strings))
(defvar *create-scanner*  (symbol-function 'compile-regex))

;; end of cl-ppcre-interface.lisp 
