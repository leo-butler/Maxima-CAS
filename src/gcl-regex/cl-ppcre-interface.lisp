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
(defvar *default-register-number* 10)
(defun count-registers (regex)
  (if (stringp regex)
      (length (remove-if-not (lambda(x) (char= x #\()) (coerce regex 'list)))
      *default-register-number*))
(defparameter *newline-as-char-list*
  '(
    #+windows
    #\Return#\Newline  ;; CR=13, LF=10
    #+macintosh
    #\Return           ;; CR
    #+unix
    #\Newline          ;; LF
    )
  )
(defparameter *space-class-list* `(#\  #\Tab ,@*newline-as-char-list*))
(defparameter *complemented-space-class-list* (append '(#\^) *space-class-list*))
(defparameter *dot-class-list* `(#\^ ,@*newline-as-char-list*))
(defparameter *digit-class-list* '(#\0 #\- #\9))
(defparameter *space-class* (coerce *space-class-list* 'string))
(defparameter *newline* (coerce *newline-as-char-list* 'string))

(defun hexadecimal-digit-p (c)
  (and c (or (and (char>= c #\0) (char<= c #\9))
	     (and (char>= c #\A) (char<= c #\F))
	     (and (char>= c #\a) (char<= c #\f)))))
(defun octal-digit-p (c)
  (and c (char>= c #\0) (char<= c #\7)))
(defun binary-digit-p (c)
  (and c (or (char= c #\0) (char= c #\1))))
(defun digitp (c)
  (and c (char>= c #\0) (char<= c #\9)))

(defun translate-special-chars (s)
  (declare (special *newlineas-char-list* *space-class-list*))
  (let ((s (if (consp s) s (coerce s 'list))))
    (macrolet ((setf-append (a b) `(let ((c (append '(#\]) (reverse ,b) '(#\[))))
				     (setf ,a (if ,a (append c ,a) c))))
	       (parse-nal ()
		 `(lambda (str acc)
		    (do* ((str str       (cdr str))
			  (c   (car str) (car str))
			  (x   (list c)  (if (funcall digitp-fn c) (push c x) x)))
			 ((not (funcall digitp-fn c))
			  (progn
			    (push (code-char (parse-integer (coerce (reverse x) 'string) :radix radix)) acc)
			    (values str acc)))))))
      (labels ((t-n (str acc)
		 ;;(format t "~a ~a~%" str acc)
		 (let ((c (car str)))
		   (case c
		     (#\\
		      (let ((next (cadr str)))
			(case next
			  (#\d
			   (setf-append acc *digit-class-list*))
			  (#\n
			   (setf-append acc *newline-as-char-list*))
			  (#\s
			   (setf-append acc *space-class-list*))
			  (#\S
			   (setf-append acc *complemented-space-class-list*))
			  (#\t
			   (push #\Tab acc))
			  (#\r
			   (push #\Return acc))
			  (#\f
			   (push #\Page acc))
			  (#\0
			   (multiple-value-bind (digitp-fn radix)
			       (case (caddr str)
				 ((#\x #\X)
				  (setf str (cdddr str))
				  (values 'hexadecimal-digit-p 16))
				 ((#\b #\B)
				  (setf str (cdddr str))
				  (values 'binary-digit-p 2))
				 ((#\o #\q #\O #\Q)
				  (setf str (cdddr str))
				  (values 'octal-digit-p 8))
				 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
				  (setf str (cddr str))
				  (values 'octal-digit-p 8))
				 ((#\d #\D)
				  (setf str (cdddr str))
				  (values 'digitp 10))
				 (otherwise
				  (let ((errmsg (format nil (intl:gettext "Malformed escaped digit ~a.") str)))
				    (error errmsg))))
			     (multiple-value-setq (str acc) (funcall (parse-nal) str acc))
			     (return-from t-n (t-n str acc))))
			((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
			 (let ((digitp-fn 'octal-digit-p)
			       (radix 8))
			   (setf str (cdr str))
			   (multiple-value-setq (str acc) (funcall (parse-nal) str acc))
			   (return-from t-n (t-n str acc))))
			(#\\
			 (push #\\ acc))
			(otherwise
			 (push #\\ acc)
			 (push next acc))))
		     (t-n (cddr str) acc))
		   (#\.
		    (setf-append acc *dot-class-list*)
		    (t-n (cdr str) acc))
		   ((nil)
		    (return-from t-n acc))
		   (otherwise
		    (push c acc)
		    (t-n (cdr str) acc))))))
      (coerce (reverse (t-n s nil)) 'string)))))
(defun compile-regex (re)
  (translate-special-chars re))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pregexp scan and scan-to-strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scan (re str &key (start 0) (end (length str)))
  ;;(format *terminal-io* "~&~s" re)
  (setf re (translate-special-chars re))
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
  (setf re (translate-special-chars re))
  (let (b e mb me)
    (multiple-value-setq (b e mb me) (scan re str :start start :end end))
    (if (null b) (return-from scan-to-strings nil))
    (let* ((m (subseq str b e))
	   (l (length mb))
	   (match-strings (make-array l :element-type 'string :initial-element #\^@)))
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
