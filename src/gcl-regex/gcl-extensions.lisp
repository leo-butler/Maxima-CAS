;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

;; (eval-when (compile load eval)
;;   (defpackage :gcl-extensions
;;     (:use :common-lisp)
;;     (:export #:read-sequence
;; 	     #:with-standard-io-syntax
;; 	     )))

(declaim (inline read-sequence))
(defun read-sequence (s in &key (start 0) end)
  (setf end (or end (length s)))
  (dotimes (i (- end start))
    (setf (aref s i) (read-byte in))))

(defmacro with-standard-io-syntax (&body body)
  (let ((l '((*package*                    (find-package :cl-info))
	     (*print-array*                t                                   )
	     (*print-base*                 10                                  )
	     (*print-case*                 :upcase                             )
	     (*print-circle*               nil                                 )
	     (*print-escape*               t                                   )
	     (*print-gensym*               t                                   )
	     (*print-length*               nil                                 )
	     (*print-level*                nil                                 )
	     (*print-lines*                nil                                 )
	     (*print-miser-width*          nil                                 )
	     ;;(*print-pprint-dispatch*      The standard pprint dispatch table  )
	     (*print-pretty*               nil                                 )
	     (*print-radix*                nil                                 )
	     (*print-readably*             t                                   )
	     (*print-right-margin*         nil                                 )
	     (*read-base*                  10                                  )
	     ;;(*read-default-float-format*  single-float                        )
	     (*read-eval*                  t                                   )
	     (*read-suppress*              nil                                 )
	     ;;(*readtable*                  The standard readtable              )
	     )))
    (let ((v (mapcar #'car l)))
      `(let ,l
	 (declare (ignorable ,@v))
	 ,@body))))

;; end of extensions.lisp 
