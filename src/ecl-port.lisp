(in-package :maxima)

#+ecl (eval-when (:execute)
  (compile 'maxima::make-unspecial
	   '(lambda (s)
	     (when (symbolp s)
	       (format t "~%;;; Declaring ~A as NOT SPECIAL" s)
	       (ffi::c-inline (s) (:object) :object
		     "
if ((#0)->symbol.stype==stp_special)
	(#0)->symbol.stype &= ~stp_special;"
		     :one-liner nil)
	       s))))

#+ecl (eval-when (:load-toplevel)
  (defun maxima::make-unspecial (s)
    (when (symbolp s)
      (format t "~%;;; Declaring ~A as NOT SPECIAL" s)
      (ffi::c-inline (s) (:object) :object
		     "
if ((#0)->symbol.stype & stp_special)
	(#0)->symbol.stype &= ~stp_special;"
		     :one-liner nil)
      s)))

#+ecl (si::trap-fpe 'floating-point-underflow nil)

;;;
;;; The following optimizers are here because we sometimes run maxima
;;; interpreted. They will change when some of these optimizations get
;;; in ECL's compiler -- in that case we will simply reuse its code.
;;;

#+ecl
(progn
  (format t "~&;;; SHADOWING COERCE")
  (shadow 'cl:coerce)
  (intern "COERCE")
  (shadow 'cl:typep)
  (intern "TYPEP"))

#+ecl
(defmacro maxima::typep (object type &environment env)
  (let* ((whole (list 'cl:typep object type))
	 (fd (compiler-macro-function 'cl:typep)))
    (format *error-output* "~%;;; Optimizing (TYPEP~{ ~S~})" (rest whole))
    (let* ((output (funcall fd whole env)))
      (format *error-output* " -> ~S" output)
      (subst 'cl:typep 'typep output))))

#+ecl
(defmacro coerce (&whole whole object type &environment env)
  (let* ((whole (list* 'cl:coerce (rest whole)))
	 (fd (compiler-macro-function 'cl:coerce)))
    (format *error-output* "~%;;; Optimizing (COERCE~{ ~S~})" (rest whole))
    (let* ((output (funcall fd whole env)))
      (format *error-output* " -> ~S" output)
      (subst 'cl:coerce 'coerce output))))