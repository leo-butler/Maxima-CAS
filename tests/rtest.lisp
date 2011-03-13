;; Copyright Leo Butler 2010
;; Released under the terms of GPLv3
;;
;; Reversion tests for build-info.lisp
;;
;; $Id: rtest.lisp,v 1.2 2011/01/17 23:23:31 work Exp $

(defpackage #:rtest
  #-gcl(:use #:cl)
  #+gcl(:use #:cl #:pcl)
  (:export #:defrtest
	   #:simple-defrtest
	   #:do-and-report-tests
	   #:do-tests
	   #:reset
	   #:report-summary
	   #:check
	   #:test)
  #+gcl(:import-from #:pcl #:lambda-closure)
  (:documentation "Regression tester"))

(in-package :rtest)

(defclass test ()
  ((tpasses      :accessor test-passes        :initarg :tpasses      :initform 0   :allocation :class)
   (tfails       :accessor test-fails         :initarg :tfails       :initform '() :allocation :class)
   (tnumber      :accessor test-number        :initarg :tnumber      :initform 0   :allocation :class)
   (tname        :accessor test-name          :initarg :tname        :initform '() :allocation :class)
   (tests        :accessor test-tests         :initarg :tests        :initform '() :allocation :class)))

(defmacro report-error-and-continue (x &body body)
  (let ((results (gensym))
	(condition (gensym)))
    `(let ((,results (multiple-value-list (ignore-errors ,@body))))
       (if (and (null (first ,results))
		(typep (second ,results) 'condition)
		(null (nthcdr 2 ,results)))
	   (let ((,condition (second ,results)))
	     (format t "~&Test name: ~a~%" (or (rtest-name ,x) (rtest-number ,x)))
	     (typecase ,condition
	       (simple-condition
		(apply #'format t
		       (simple-condition-format-control ,condition)
		       (simple-condition-format-arguments ,condition)))
	       (otherwise
		(format t "~A error." (type-of ,condition))))
	     nil)
	   (values-list ,results)))))

(defmethod add-test ((x test) form)
  (setf (test-tests x) (push (cons form nil) (test-tests x))))
(defmethod do-tests ((x test) fn)
  (let ((tests (reverse (test-tests x)))
	(more nil))
    (dolist (i tests)
      (cond ((and (consp i) (null (cdr i)))
	     (setf more t (cdr i) t)
	     (report-error-and-continue (car i) (funcall fn (car i))))
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
   (source       :accessor rtest-source       :initarg :source       :initform '())
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
	(setf (test-fails x) (push (list (test-number x) expect (rtest-source x)) (test-fails x))))))
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

(defmethod deliberate-fails ((x test))
  (count 'deliberate-fail (mapcar #'second (test-fails x))))

(defmethod report-summary ((x test) &optional (ignore-deliberate-fails t))
  (format t "~&Test name: ~a~%" (test-name x))
  (format t "~&Passes/Intended Fails/Tests: ~a/~a/~a" (test-passes x) (deliberate-fails x) (test-number x))
  (let ((fails (if ignore-deliberate-fails
		   (loop for k in (test-fails x) unless (eql (cadr k) 'deliberate-fail) collect k)
		   (test-fails x))))
    (format t "~&~:[Fails:  ~a~%~;~]" (eq fails nil) fails))
  t)

(defmethod check ((x rtest) &optional (do-report nil))
  (tests++ x)
  (flet ((compute-tests (f in)
	   (cond ((and (listp f) (null in) (not (eq (car f) 'lambda-closure)))
		  (mapcar #'(lambda(fn) (report-error-and-continue x (funcall fn))) f))
		 ((and (listp f) (not (eq (car f) 'lambda-closure)))
		  (mapcar #'(lambda(fn inp) (report-error-and-continue x (apply fn inp))) f in))
		 ((functionp f)
		  ;;(format t "~&f = ~A~%" f)
		  (report-error-and-continue x (apply f in)))
		 (t
		  f))))
    (let* ((inputs (rtest-inputs x))
	   (function (rtest-func x))
	   (test (rtest-equality-fn x))
	   (answer (rtest-answer x))
	   (report (rtest-report x))
	   (computed-result (compute-tests function inputs))
	   (y-n             (funcall test computed-result answer)))
      ;;(format t "~A~%~A~%" computed-result answer)
      (if y-n (passes++ x) (fails++ x))
      (if report
	  (report-results x :outcome y-n :computed-result computed-result))
      (if do-report (report x (testname x)))
      y-n)))

(defmacro defrtest (y &rest opts)
  (if (and (consp y) (eq (car y) 'quote))
      (setq y (cadr y)))
  (let ((source (loop for (k v) on opts by #'cddr
		   when (eql k :func) return v)))
    (let ((dfrt `(lambda (&rest opts)
		   (setq opts (append (list 'rtest)
				      (loop for (k v) on opts by #'cddr
					 collect k collect v)
				      '(:expect pass)
				      '(:source ,source)))
		   (setf ,y (apply #'make-instance opts))
		   (setf (rtest-current ,y) ,y)
		   (add-test ,(get-test) ,y)
		   ,y
		   )))
      `(funcall ,dfrt ,@opts))))

(defmacro simple-defrtest (x in-form answer-form)
  `(defrtest ,x :func #'(lambda() (equalp ,in-form ,answer-form))))

(defmacro do-and-report-tests (&rest forms)
  "Takes a list of pairs of forms, defines an rtest for each pair,
runs the tests and reports the results."
  (let ((tests (loop for (in out) on forms by #'cddr
		  collect `(simple-defrtest x ,in ',out))))
    `(progn
       (defparameter x (make-instance 'test :tname "API"))
       ,@tests
       (do-tests x #'check)
       (report-summary x)
       (reset x))))

