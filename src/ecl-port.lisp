#+ecl (eval-when (:execute)
  (compile 'maxima::make-unspecial
	   '(lambda (s)
	     (when (symbolp s)
	       (format t "~%;;; Declaring ~A as NOT SPECIAL" s)
	       (ffi::c-inline (s) (:object) :object
		     "
if ((#0)->symbol.stype==stp_special)
	(#0)->symbol.stype = stp_ordinary;"
		     :one-liner nil)
	       s))))

#+ecl (eval-when (:load-toplevel)
  (defun maxima::make-unspecial (s)
    (when (symbolp s)
      (format t "~%;;; Declaring ~A as NOT SPECIAL" s)
      (ffi::c-inline (s) (:object) :object
		     "
if ((#0)->symbol.stype==stp_special)
	(#0)->symbol.stype = stp_ordinary;"
		     :one-liner nil)
      s)))

#+ecl (si::trap-fpe 'floating-point-underflow nil)
