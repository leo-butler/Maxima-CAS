;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

(load "gcl-regex.lisp")
(pushnew :gcl-regex *features*)
(load "cl-ppcre-interface.lisp")
(load "api.lisp")
(load "../../tests/rtest.lisp")
(in-package :gcl-regex)
(load "../../tests/rtest-api.lisp")

;; Test translate-special-chars
(defun fmt (upcase prefix radix)
  "FMT (UPCASE PREFIX RADIX) Creates a FORMAT format string of the
form \\0x~3,'0~x where the prefix \\0x is optional when x=o; x is
determined by RADIX. This format string is wrapped by an
upcase/downcase clause determined by UPCASE."
  (unless (eq radix 'o) (setf prefix t))
  (format nil "~[~~(~;~~:(~]\\~[0~a~1:*~;~]~~3,'0~a~~)"
	  (if upcase 0 1)
	  (if prefix 0 1)
	  radix))

(defun test-translation (upcase prefix radix-sym)
  "Loops over all 256 characters and their incarnation as escaped
octal, binary, etc. numbers. Collects failed conversions, so the
expected result is NIL."
  (let ((fmt (fmt upcase prefix radix-sym)))
    (loop for i from 0 to 255
       for p = (format nil "~a" (code-char i))
       for s = (format nil fmt i)
       for q = (translate-special-chars s)
       when (not (string= p q)) collect (list p q)
       when nil do (format t "(~a ~a ~a) " i p s))))

(rtest:do-and-report-tests
    ;;
    ;; sanity check for fmt
    (loop for u in '(nil t)
       collect (loop for p in '(nil t)
		  collect (loop for r in '(o d x b)
			     collect (fmt u p r))))
  ((("~:(\\~3,'0O~)" "~:(\\0D~3,'0D~)" "~:(\\0X~3,'0X~)"
		     "~:(\\0B~3,'0B~)")
    ("~:(\\0O~3,'0O~)" "~:(\\0D~3,'0D~)" "~:(\\0X~3,'0X~)"
		       "~:(\\0B~3,'0B~)"))
   (("~(\\~3,'0O~)" "~(\\0D~3,'0D~)" "~(\\0X~3,'0X~)" "~(\\0B~3,'0B~)")
    ("~(\\0O~3,'0O~)" "~(\\0D~3,'0D~)" "~(\\0X~3,'0X~)" "~(\\0B~3,'0B~)")))
  ;;
  (loop for u in '(nil t)
     collect (loop for p in '(nil t)
		collect (loop for r in '(o d x b)
			   collect (test-translation u p r))))
  ;;
  (((nil nil nil nil) (nil nil nil nil))
   ((nil nil nil nil) (nil nil nil nil)))
  ;;
  )

;; end of rtest-run.lisp 
