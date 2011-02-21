;; Copyright Leo Butler 2010
;; Released under the terms of GPLv3
;;
;; Reversion tests for build-info.lisp
;;
;; $Id: rtest-build-index.lisp,v 1.9 2011/01/28 01:52:17 work Exp work $

(load "../src/maxima-package.lisp")
(load "rtest.lisp")
(load "../src/nregex.lisp")
(load "../src/cl-info.lisp")
(load "../src/build-index.lisp")
(in-package :maxima-nregex)
(use-package :rtest)

(defparameter *debug* nil)
(defmacro info (a &body b) (if *debug* `(format t ,a ,@b)))
(defmacro simple-defrtest (x in-form answer-form)
  `(defrtest ,x :func #'(lambda() ,in-form) :answer ,answer-form :equality-fn #'equalp))
(defun cl-ppcre-to-nregex (list)
  (let (nregex-name)
    (mapcar #'(lambda(name)
		(setf nregex-name (intern (concatenate 'string "NREGEX-" (symbol-name name)) :maxima-nregex))
		(if (functionp nregex-name)
		    (eval `(defun    ,name (&rest args) (apply #',nregex-name args)))))
	    list)))
(cl-ppcre-to-nregex '(scan scan-to-strings register-groups-bind do-register-groups all-matches all-matches-as-strings))


(defparameter x (make-instance 'test :tname "nregex" :tpasses 0 :tfails nil :tnumber 0))

(simple-defrtest x (multiple-value-list (scan "(a*)b" "xaaabd"))
	        (list 1 5 #(1) #(4)))

(simple-defrtest x (multiple-value-list (scan "(a*)b" "xaaabd" :start 1))
	        (list 1 5 #(1) #(4)))

(simple-defrtest x (multiple-value-list (scan "(a*)b" "xaaabd" :start 2))
	        (list 2 5 #(2) #(4)))

;;(null (scan "(a*)b" "xaaabd" :end 4))
;;
;;;;(equalp (multiple-value-list (scan '(:greedy-repetition 0 nil #\b) "bbbc"))
;;;;	        (list 0 3 #() #()))
;;
;;;;(null (scan '(:greedy-repetition 4 6 #\b) "bbbc"))
;;
;;;;(let ((s (create-scanner "(([a-c])+)x")))
;;;;    (equalp (multiple-value-list (scan s "abcxy"))
;;;;	              (list 0 4 #(0 2) #(3 3))))
;;
(simple-defrtest x (multiple-value-list (scan-to-strings "[^b]*b" "aaabd"))
	        (list "aaab" #()))

(simple-defrtest x (multiple-value-list (scan-to-strings "([^b]*)b" "aaabd"))
	        (list "aaab" #("aaa")))

(simple-defrtest x (multiple-value-list (scan-to-strings "(([^b]*)b)" "aaabd"))
	        (list "aaab" #("aaab" "aaa")))


;;(simple-defrtest x `(list ,(nregex-register-groups-bind (first second third fourth)
;;						 ("((a)(b)(c))" "abababc" :sharedp t)
;;						 (list first second third fourth)))
;;		  (list "c" "a" "b" "c"))
;;
;;;;(simple-defrtest x (register-groups-bind (nil second third fourth)
;;;;			                  ("((a)|(b)|(c))()+" "abababc" :start 6)
;;;;					            (list second third fourth))
;;;;		 (list nil nil "c"))
;;;;
;;;;(null (register-groups-bind (first)
;;;;			              ("(a|b)+" "accc" :start 1)
;;;;				              first))
;;;;
;;;;(simple-defrtest x (register-groups-bind (fname lname (#'parse-integer date month year))
;;;;			                  ("(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})" "Frank Zappa 21.12.1940")
;;;;					            (list fname lname (encode-universal-time 0 0 0 date month year 0)))
;;;;	        (list "Frank" "Zappa" 1292889600))
;;;;

(let ((mi (cl-info::slurp-info-file "rtest-build-index/en/maxima.info"))
      (maxima-info-re "(maxima.info)-([0-9]+)"))
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