;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.215 2009/04/07 22:05:21 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.200 2009/01/19 02:38:17 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.112 2009/01/08 12:57:19 rtoy Exp $")

;;; Using Lisp CMU Common Lisp 19f (19F)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :lapack)


(let* ((one 1.0) (zero 0.0))
  (declare (type (double-float 1.0 1.0) one)
           (type (double-float 0.0 0.0) zero)
           (ignorable one zero))
  (defun dlarf (side m n v incv tau c ldc work)
    (declare (type (double-float) tau)
             (type (array double-float (*)) work c v)
             (type (f2cl-lib:integer4) ldc incv n m)
             (type (simple-array character (*)) side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (v double-float v-%data% v-%offset%)
         (c double-float c-%data% c-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ()
        (declare)
        (cond
          ((lsame side "L")
           (cond
             ((/= tau zero)
              (dgemv "Transpose" m n one c ldc v incv zero work 1)
              (dger m n (- tau) v incv work 1 c ldc))))
          (t
           (cond
             ((/= tau zero)
              (dgemv "No transpose" m n one c ldc v incv zero work 1)
              (dger m n (- tau) work 1 v incv c ldc)))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlarf fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-array character (1))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dger fortran-to-lisp::dgemv
                    fortran-to-lisp::lsame))))

