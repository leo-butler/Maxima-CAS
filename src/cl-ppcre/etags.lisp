;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

(eval-when #-gcl(:load-toplevel :execute)
	   #+gcl(load eval)
	   (defpackage :etags
	     (:use :cl :cl-fad :regex)
	     (:export #:etags-create-tags
		      #:etags-create-tags-recursive)
	     (:import-from :regex
			   #:do-register-groups
			   #:split
			   )
	     (:import-from :cl-fad
			   #:file-exists-p
			   #:walk-directory
			   #:file-pathname-p
			   #:directory-pathname-p
			   #:slurp
			   )
	     ))

(in-package :etags)
(defvar *etags-eos* "" "End of section marker")          ;;  12
(defvar *etags-eod* "" "End of tag definition text")     ;;  127
(defvar *etags-eot* "" "End of tag")                     ;;  1
(defvar *etags-eor* "\\n" "End of regexp marker")
(defvar *etags-regex-list* nil "Current REGEX list.")
(defvar *etags-exclude-subdirs* '("." ".." "Attic" "CVS" "RCS" ".git" ".svn") "List of directories to be excluded from searches")
(defvar *etags-max-recursion-depth* 1000)
(defvar *etags-last-file-regex-list* '() "Most recently used file-regex-list")
(defvar *etags-last-directory-list* '() "Most recently used directory-list")
(defvar *etags-last-regex-list* '() "Most recently used regex-list")
(defvar *etags-tags-file* (make-pathname :directory
					 #+gcl '(".") #-gcl '(:relative)
					 :name "TAGS" :type nil)
  "Default TAGS file pathname.")
(defvar *etags-regex-lisp-list*
  '("[ \\t]*\\(def(un|mfun|macro|var|parameter|advice|package)\\s+(\\S+)" 2
    "[ \\t]*\\(define-(\\S+)\\s+(\\S+)" 2
    )
  "List of LISP ETAGS regexes structured as [REGEX REGISTER]* where
  REGISTER is the number of the capture register (counting from 1) to
  use in constructing the TAGS file. At the moment, GCL does not allow
  cloisters (non-capturing registers).")
(defparameter *etags-regex-maxima-list*
  (let* ((name        "([a-zA-Z_%]+)")        ;; capture the function name
	 (open-paren  "\\(")               ;; both function and array functions
	 (close-paren "\\)")               ;; this is lazy, but faster
	 (open-brack  "\\[")
	 (close-brack "\\]")
	 (no-paren    "[^\\)]*")
	 (no-brack    "[^]]*")
	 (space      "\\s*")
	 (args       (concatenate 'string "(" open-paren no-paren close-paren "|" open-brack no-brack close-brack ")"))
	 (regex      (concatenate 'string "[ \\t]*" name space args space ":?:=")))
    (list
     regex 1
     ))
  "List of MAXIMA ETAGS regexes structured as [REGEX REGISTER]* where
  REGISTER is the number of the capture register (counting from 1) to
  use in constructing the TAGS file. At the moment, GCL does not allow
  cloisters (non-capturing registers).")

(defmacro etags-ensure-list (x)
  "Ensure X is a list."
  `(unless (consp ,x) (setq ,x (list ,x))))

(defun etags-load-regexps (file-list)
  "Reads in a list of CL-PPCRE regexps in the files in `FILE-LIST'. Each
regexp is ended by the rubout character `' (see `*etags-eor*'). Lines
beginning with `;' are ignored. If you must match `;' as the first
character in your regexp, use the single character class `[;]'. The
result is saved in `*etags-regex-list*'."
  (etags-ensure-list file-list)
  (let ((regex-list '()))
      (dolist (file file-list)
	  (dolist (i (split *etags-eor* (slurp file)))
	    (push i regex-list)))
      (setf *etags-regex-list* (mapcar (lambda(x) (if (scan "^[0-9]+$" x) (parse-integer x) x))
				     (reverse (remove-if #'(lambda(x) (or (string= "" x) (scan "^;" x))) regex-list))))))

;; ELISP functions
(defun expand-file-name (file)
  (cond ((stringp file)
	 (if (char= (char file 0) #\~)
	     (setq file (concatenate 'string (maxima::maxima-getenv "HOME") (subseq file 1)))
	     file))
	((pathnamep file)
	 (expand-file-name (namestring file)))
	(t (error "EXPAND-FILE-NAME FILE: arg must be a string or pathname. Given: ~s" file)))
  (if (directory-pathname-p file)
      (pathname-as-directory file)
      (pathname-as-file file)
  ))

(defun file-name-nondirectory (file)
  (let ((path (expand-file-name file)))
    (concatenate 'string (pathname-name path) "." (pathname-type path))))

(defun file-name-as-directory (file)
  (let ((path (expand-file-name file)))
    (pathname-directory path)))

(defun concat (&rest strings)
  (let ((string ""))
    (dolist (s strings string)
       (setf string (concatenate 'string string s)))))

(defun match-string (pos reg-starts reg-ends string)
  (subseq string (aref reg-starts pos) (aref reg-ends pos)))

(defun etags-search-for-tags (file &optional regex-list verbose)
  "Create a string of TAGS from the source file `FILE' and the
`REGEX-LIST'. The default value of `REGEX-LIST' is stored in
`*etags-regex-list*'."
  (declare (ignorable verbose))
  (setq file (expand-file-name file)
	regex-list (or regex-list *etags-regex-list*))
  (flet ((slurp-file (f)
	   (handler-case
	       (slurp f)
	     (error () (warn "SLURP-FILE: UNABLE to open ~A." (namestring f))))))
    (let* ((out-string "")
	   tag-text tag
	   (line 0)
	   (last-start 0)
	   (out-header (format nil "~a~%~a" *etags-eos* (namestring file)))
	   (buffer (slurp-file file))
	   )
      (when (null regex-list)
	(warn (intl:gettext "ETAGS-SEARCH-FOR-TAGS FILE REGEX-LIST -> REGEX-LIST is empty.")))
      (loop :for (regex register) :on regex-list :by #'cddr
	 :do
	   (setf regex (format nil "~%(~a)" regex))
	   (and verbose (format t "~s~%" regex))
	   (do-scans (start end reg-starts reg-ends regex buffer)
	     (setf tag-text (match-string 0 reg-starts reg-ends buffer)
		   tag (match-string register reg-starts reg-ends buffer)
		   line (+ line (/ (length (all-matches "\\n" buffer :start last-start :end start)) 2))
		   last-start start)
	     (setf out-string (format nil "~a~a~a~a~a~a,~a~%" out-string tag-text *etags-eod* tag *etags-eot* (+ 2 line) (+ 2 start)))))
      (setf out-string (format nil "~a,~a~%~a" out-header (length out-string) out-string)))))

(defun etags-create-tags (file-list &optional
			  (regex-list *etags-regex-maxima-list*)
			  (TAGS *etags-tags-file*)
			  (append-or-supersede :supersede)
			  (verbose nil)
			  )
 "Create a TAGS file named by `TAGS' from the source files listed in
`FILE-LIST' using the regexes in `REGEX-LIST'."
 (etags-ensure-list file-list)
 (case append-or-supersede
   ((:append :supersede))
   ((t)
    (setf append-or-supersede :append))
   ((nil)
    (setf append-or-supersede :supersede))
   (otherwise
    (error (intl:gettext "ETAGS-CREATE-TAGS: option APPEND-OR-SUPERSEDE must be one of :append or :supersede."))))
 (with-open-file (tags-buffer TAGS :direction :output :if-exists append-or-supersede)
   (dolist (file file-list)
     (princ (etags-search-for-tags file regex-list verbose) tags-buffer))))

(defun etags-collect-files (file-regex-list &optional
			    (directory-list '(#p"./"))
			    (file-list nil)
			    (recursion-depth most-positive-fixnum)
			    )
  (flet ((join-strings (&rest strings)
		      (format nil "~{~{~A~^|~}~}" strings))
	 (escape-dot (s)
	   (coerce (loop :for c :in (coerce s 'list)
		      :when (char= c #\.) collect #\\ collect c) 'string)))
    (let ((re (concatenate 'string "(" (escape-dot (join-strings *etags-exclude-subdirs*)) ")$")))
      (flet ((directory-pathname-p (d)
	       (and (directory-pathname-p d) (not (scan re (namestring d)))))
	     (file-pathname-p (f)
	       (and (file-pathname-p f) (some (lambda(rgx) (scan rgx (namestring f))) file-regex-list))))
	(unless directory-list (setf directory-list '(#p"./"))) ;; gcl ??
	(etags-ensure-list directory-list)
	(etags-ensure-list file-regex-list)
	(labels ((collect-files (directory rd)
		   (let* ((dir-list (list-directory directory))
			  (sub-dirs (loop :for f :in dir-list
				       :if (file-pathname-p f) :do (push f file-list)
				       :else :when (directory-pathname-p f) :collect f)))
		     (unless (and rd (< 0 (decf rd))) (return-from collect-files nil))
		     (dolist (d sub-dirs)
		       (collect-files d rd)))))
	  ;;(format t "~a~%" (list directory-list recursion-depth))
	  (dolist (directory directory-list)
	    (collect-files directory recursion-depth))
	  file-list)))))

(defun etags-create-tags-recursive (&key file-regex-list directory-list regex-list TAGS verbose recursion-depth append-or-supersede)
  (etags-create-tags
   (etags-collect-files file-regex-list directory-list '() recursion-depth)
   regex-list TAGS append-or-supersede verbose))

;; end of etags.el 
