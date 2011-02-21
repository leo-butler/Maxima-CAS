;; Copyright Leo Butler 2010
;; Released under the terms of GPLv3
;;
;; Reversion tests for build-info.lisp
;;
;; $Id: rtest-test.lisp,v 1.1 2011/01/08 03:22:40 work Exp $

;;
;; Check test/rtest functions
;;

(in-package :rtest)

(defparameter x (make-instance 'test :tname "(r)test checks 1" :tpasses 0 :tfails nil :tnumber 0))
(defrtest 'x)
(defrtest 'x :func #'(lambda () t))
(defrtest 'x :func #'(lambda () nil) :expect 'deliberate-fail)
(defrtest 'x :func #'(lambda () (error "Deliberate error, tests should continue.")) :expect 'deliberate-fail :name 'deliberate-error)
(defrtest 'x :func #'(lambda () 'a) :answer 'a)
(defrtest 'x :func #'(lambda ()
		       (do-tests x #'check)
		       (not(some #'(lambda(x)(eq x 'pass)) (mapcar #'second (test-fails x))))) :answer t) ;'((3 . deliberate-fail)))
(do-tests x #'check)
(report-summary x)
(reset x)

(defrtest 'x :func #'(lambda ()
		       (let ((x (make-instance 'test)))
			 (defrtest 'x :func #'(lambda () nil) :expect 'deliberate-fail))
		       t)
	  :answer t
	  :tname "(r)test check - recursive")
(do-tests x #'check)
(report-summary x)
(reset x)

(let ((i))
  (defun deliberate-failure () (if i (incf i) (setq i 1)) (format nil "deliberate-fail-~a" i)))

(defrtest 'x :tname "(r)test checks 2"
	  :func #'(lambda ()
		       (let ((x (make-instance 'test)))
			 (defrtest 'x)
			 (defrtest 'x :func #'(lambda () t))
			 (defrtest 'x :func #'(lambda () nil) :expect 'deliberate-fail)
			 (defrtest 'x :func #'(lambda () 'a) :answer 'a)
			 (defrtest 'x :func #'(lambda ()
						(let ((x (make-instance 'test)))
						  ;; this test fails intentionally
						  (defrtest 'x :func #'cons :inputs '(a b) :equality-fn #'equal :answer '(b . a) :expect 'deliberate-fail)
						  'x
						  ))
				   :answer 'x)
			 (defrtest 'x
			     :func (list
				    #'(lambda () t)
				    #'(lambda () 'a)
				    #'(lambda ()
					(let ((x (make-instance 'test)))
					  (defrtest 'x :func #'cons :inputs '(a b) :equality-fn #'equal :answer '(a . b))
					  'x
					  ))
				    #'(lambda ()
					(let ((x (make-instance 'test)))
					  ;; this test fails intentionally
					  (defrtest 'x :func #'cons :inputs '(a b) :equality-fn #'equal :answer '(b . a) :expect 'deliberate-fail)
					  'x
					  )))
			     :answer '(t a x x))
			 )
		       ;(format t "~a~%" (test-fails x))
		       ;(test-fails x)
		       t)
	  :answer t;'((10 . DELIBERATE-FAIL) (8 . DELIBERATE-FAIL) (4 . DELIBERATE-FAIL))
	  :expect 'pass
	  :equality-fn #'equal)

(do-tests x #'check)
(report-summary x)
(reset x)
