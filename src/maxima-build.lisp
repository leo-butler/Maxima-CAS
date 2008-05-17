(load "../lisp-utils/defsystem.lisp")
#+ecl (load "maxima-package.lisp")
#+ecl
(compile 'maxima::make-unspecial
	 '(lambda (s)
	   (when (symbolp s)
	     (format t "~%;;; Declaring ~A as NOT SPECIAL" s)
	     (ffi::c-inline (s) (:object) :object
			    "((#0)->symbol.stype = stp_ordinary, #0)"
			    :one-liner t))))

(defun maxima-compile ()
  (mk:oos "maxima" :compile))
(defun maxima-load ()
  (mk:oos "maxima" :load))
(defun maxima-dump ()
  #+clisp(ext:saveinitmem "binary-clisp/maxima.mem" 
		   :init-function (function cl-user::run)))
