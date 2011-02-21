;; Copyright Leo Butler 2010
;; Released under the terms of GPLv3
;;
;; Reversion tests for build-info.lisp
;;
;; $Id: rtest-build-index.lisp,v 1.9 2011/01/28 01:52:17 work Exp work $

(load "maxima-package.lisp")
(load "../tests/rtest.lisp")
(load "nregex.lisp")
(load "cl-info.lisp")
(load "build-index.lisp")
(in-package :maxima-nregex)
(use-package :rtest)

(defparameter *debug* t)
(defmacro info (a &body b) (if *debug* `(format t ,a ,@b)))

(defparameter x (make-instance 'test :tname "nregex" :tpasses 0 :tfails nil :tnumber 0))

(let ((mi (cl-info::slurp-info-file "../tests/rtest-build-index/en/maxima.info"))
      (maxima-info-re "(maxima.info)-([0-9]+)"))
  (info "~a" mi)
  (defrtest 'x :func #'(lambda(re str)
			 (let (b e b-r e-r)
			   (multiple-value-setq (b e b-r e-r) (nregex-scan re str))
			   (info "~A~%~A~%" (list b e b-r e-r) '(328 341 #(328 340) #(339 341)))
			   (list b e b-r e-r)))
	    :inputs (list maxima-info-re mi)
	    :answer '(328 341 #(328 340) #(339 341))
	    :equality-fn #'equalp)
  
  (defrtest 'x :func #'(lambda(re str)
			 (let (m r)
			   (multiple-value-setq (m r) (nregex-scan-to-strings re str))
			   (info "~A~%" (list m r))
			   (list m r)))
	    :inputs (list maxima-info-re mi)
	    :answer '("maxima.info-1" #("maxima.info" "1"))
	    :equality-fn #'equalp)
)

(do-tests x #'check)
(report-summary x)