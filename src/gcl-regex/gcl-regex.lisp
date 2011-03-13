
(eval-when #-gcl'()
	   #+gcl(load compile eval)
	   (defpackage :gcl-regex
	     (:use :common-lisp)
	     (:export
	      ;; Vars
	      #:*scan*
	      #:*scan-to-strings*
	      #:*create-scanner* 
	      ;; Functions
	      ;; Functions in cl-ppcre-interface.lisp
	      #:scan
	      #:scan-to-strings
	      #:count-registers
	      ;; Functions in api.lisp
	      #:register-groups-bind
	      #:do-scans
	      #:do-scans-to-strings
	      #:do-matches
	      #:do-matches-as-strings
	      #:all-matches
	      #:all-matches-as-strings
	      #:do-register-groups
	      ))
	   )
