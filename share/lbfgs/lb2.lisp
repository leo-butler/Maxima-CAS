;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.212 2009/01/08 18:58:49 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.199 2009/01/07 19:16:59 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.112 2009/01/08 12:57:19 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package :common-lisp-user)


(defstruct (lb3
             (:predicate is-lb3-p))
  (mp 0 :type (f2cl-lib:integer4))
  (lp 0 :type (f2cl-lib:integer4))
  (gtol 0.0d0 :type (double-float))
  (stpmin 0.0d0 :type (double-float))
  (stpmax 0.0d0 :type (double-float)))


(defparameter *lb3-common-block*
  (let* ()
    (declare (ignorable))
    (make-lb3)))


(defun /blockdata-lb2/ ()
  (let ()
    (symbol-macrolet ((stpmax (lb3-stpmax *lb3-common-block*))
                      (stpmin (lb3-stpmin *lb3-common-block*))
                      (gtol (lb3-gtol *lb3-common-block*))
                      (lp (lb3-lp *lb3-common-block*))
                      (mp (lb3-mp *lb3-common-block*)))
      (setf mp 6)
      (setf lp 6)
      (setf gtol 0.9d0)
      (setf stpmin 1.0d-20)
      (setf stpmax 1.0d20))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::/blockdata-lb2/
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types 'nil
                                            :return-values 'nil
                                            :calls 'nil)))

