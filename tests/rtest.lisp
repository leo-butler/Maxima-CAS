;; Copyright Leo Butler 2010
;; Released under the terms of GPLv3
;;
;; Reversion tests for build-info.lisp
;;
;; $Id: rtest.lisp,v 1.2 2011/01/17 23:23:31 work Exp $

(defpackage #:rtest
  (:use #:cl)
  (:export #:defrtest
	   #:do-tests
	   #:reset
	   #:report-summary
	   #:check
	   #:test)
  (:documentation "Regression tester"))

(in-package :rtest)

(defclass test ()
  ((tpasses      :accessor test-passes        :initarg :tpasses      :initform 0   :allocation :class)
   (tfails       :accessor test-fails         :initarg :tfails       :initform nil :allocation :class)
   (tnumber      :accessor test-number        :initarg :tnumber      :initform 0   :allocation :class)
   (tname        :accessor test-name          :initarg :tname        :initform nil :allocation :class)
   (tests        :accessor test-tests         :initarg :tests        :initform nil :allocation :class)))

(defmacro report-error-and-continue (x &body body)
  (let ((results (gensym))
	(condition (gensym)))
    `(let ((,results (multiple-value-list (ignore-errors ,@body))))
       (if (and (null (first ,results))
		(typep (second ,results) 'condition)
		(null (nthcdr 2 ,results)))
	   (let ((,condition (second ,results)))
	     (format t "~&Test name: ~a~%" (or (rtest-name (car ,x)) (rtest-number (car ,x))))
	     (typecase ,condition
	       (simple-condition
		(apply #'format t
		       (simple-condition-format-control ,condition)
		       (simple-condition-format-arguments ,condition)))
	       (otherwise
		(format t "~A error." (type-of ,condition))))
	     (values))
	   (values-list ,results)))))

(defmethod add-test ((x test) form)
  (setf (test-tests x) (push (cons form nil) (test-tests x))))
(defmethod do-tests ((x test) fn)
  (let ((tests (reverse (test-tests x)))
	(more nil))
    (dolist (i tests)
      (cond ((and (consp i) (null (cdr i)))
	     (setf more t (cdr i) t)
	     (report-error-and-continue i (funcall fn (car i))))
	    (t
	     nil)))
    (if more
	(do-tests x fn)
	'done)))
      
(defmethod reset ((x test)) (setf (test-passes x) 0
			    	  (test-fails  x) nil
				  (test-number x) 0
				  (test-name   x) nil
				  (test-tests  x) nil))
(defun get-test (&optional x)
  (if (typep x 'test) x (make-instance 'test)))
   
(defclass rtest (test)
  ((passes       :accessor rtest-passes       :initarg :passes       :initform 0)
   (fails        :accessor rtest-fails        :initarg :fails        :initform 0)
   (func         :accessor rtest-func         :initarg :func         :initform #'(lambda (&rest r) (declare (ignore r)) t))
   (name         :accessor rtest-name         :initarg :name         :initform nil)
   (number       :accessor rtest-number       :initarg :number       :initform 0)
   (number-tests :accessor rtest-number-tests :initarg :number-tests :initform t)
   (equality-fn  :accessor rtest-equality-fn  :initarg :equality-fn  :initform #'equal)
   (answer       :accessor rtest-answer       :initarg :answer       :initform t)
   (opts         :accessor rtest-opts         :initarg :opts         :initform nil)
   (report       :accessor rtest-report       :initarg :report       :initform nil)
   (verbose      :accessor rtest-verbose      :initarg :verbose      :initform nil)
   (inputs       :accessor rtest-inputs       :initarg :inputs       :initform nil)
   (expect       :accessor rtest-expect       :initarg :expect       :initform 'pass)
   (current      :accessor rtest-current :allocation :class)))

(defmethod reset ((x rtest)) (setf (rtest-passes x) 0
				   (rtest-fails x)  0
				   (rtest-number x) 0)
	   (call-next-method x)
	   t)
(defmethod fails++  ((x rtest) &optional (inc 1))
  (let* ((expect* (rtest-expect x))
	 (expect (if (fboundp expect*)
		     (funcall expect*)
		     expect*)))
    (or (equal expect 'deliberate-fail) (incf (rtest-fails x) inc))
    (if (> inc 0)
	(setf (test-fails x) (push (cons (test-number x) expect) (test-fails x))))))
(defmethod passes++ ((x rtest) &optional (inc 1)) (incf (rtest-passes x) inc) (incf (test-passes x) inc))
(defmethod tests++  ((x rtest) &optional (inc 1)) (incf (rtest-number x) inc) (incf (test-number x) inc))
(defmethod testname ((x rtest) &optional name)
  (setf (rtest-name x)
	(cond ((null (rtest-name x))
	       (or name
		   (rtest-func x)))
	      ((null name)
	       (or (rtest-name x)
		   (test-name x)
		   (rtest-func x)))
	      (t
	       name))))

(defmethod report-results ((x rtest)
			   &key
			   ((:outcome         outcome))
			   ((:computed-result computed-result))
			   )
  (let ((answer (rtest-answer x))
	(verbose (rtest-verbose x))
	(testname (testname x)))
    (format t "~&~:[FAIL~;pass~] ... ~a" outcome testname)
    (if verbose
	(format t ": (expected: ~a) (computed: ~a)" answer computed-result))
    (format t "~%")))

(defmethod report ((x rtest) &optional test-suite)
  (if test-suite (format t "Test suite: ~a~%" test-suite))
  (format t "~&Passes: ~a/~a" (rtest-passes x) (rtest-number x))
  (let ((fails (rtest-fails x)))
    (format t "~&~:[Fails:  ~a~%~;~]" (eq fails 0) fails))
  t)

(defmethod report-summary ((x test) &optional (ignore-deliberate-fails t))
  (format t "~&Test name: ~a~%" (test-name x))
  (format t "~&Passes/Tests: ~a/~a" (test-passes x) (test-number x))
  (let ((fails (if ignore-deliberate-fails
		   (loop for k in (test-fails x) unless (eql (cdr k) 'deliberate-fail) collect k)
		   (test-fails x))))
    (format t "~&~:[Fails:  ~a~%~;~]" (eq fails nil) fails))
  t)

(defmethod check ((x rtest) &optional (do-report nil))
  (tests++ x)
  (flet ((compute-tests (f in)
	   (cond ((and (listp f) (null in))
		  (mapcar #'(lambda(fn) (apply fn in)) f))
		 ((listp f)
		  (mapcar #'(lambda(fn inp) (apply fn inp)) f in))
		 (t
		  (apply f in)))))
    (let* ((inputs (rtest-inputs x))
	   (function (rtest-func x))
	   (test (rtest-equality-fn x))
	   (answer (rtest-answer x))
	   (report (rtest-report x))
	   (computed-result (compute-tests function inputs))
	   (y-n             (funcall test computed-result answer)))
      (if y-n (passes++ x) (fails++ x))
      (if report
	  (report-results x :outcome y-n :computed-result computed-result))
      (if do-report (report x (testname x)))
      y-n)))

(defun defrtest (y &rest opts)
  (let* ((u (get-test))
	 (args (loop for (k v) on opts by #'cddr
		  collect k collect `(quote ,v))))
    (eval
     `(progn
	(setf ,y (funcall #'make-instance 'rtest ,@args :expect 'pass))
	(setf (rtest-current ,y) ,y)
	(add-test ,u ,y))))
  y)
