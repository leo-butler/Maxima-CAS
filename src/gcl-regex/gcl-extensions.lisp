;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

(eval-when (load eval)
  (defpackage :gcl-extensions
    (:use :common-lisp)
    (:export #:read-sequence
	     #:with-standard-io-syntax
	     #:wild-pathname-p
	     #:probe-directory
	     #:read-byte-sequence
	     #:read-char-sequence
	     )))

(in-package :gcl-extensions)

(defmacro read-*-sequence (name read-fn)
  `(defun ,name (s in &key (start 0) end)
     "A non-optimised version of read-sequence for a character or byte
stream."
     (setf end (or end (length s)))
     (dotimes (i start)
       (,read-fn in nil nil))
     (loop for i from start to (1- end)
	for r = (,read-fn in nil nil)
	if r do
	  (setf (aref s i) r)
	else do
	  (return-from ,name (values i s)))
     (values end s)))

(read-*-sequence read-char-sequence read-char)
(read-*-sequence read-byte-sequence read-byte)
(defun read-sequence (s in &key (start 0) end)
  (let ((set (stream-element-type in)))
    (if (consp set) (setq set (car set)))
    (let ((set (symbol-name set)))
      (cond ((or (string= set "UNSIGNED")
		 (string= set "UNSIGNED-BYTE")
		 (string= set "SIGNED")
		 (string= set "SIGNED-BYTE")
		 (string= set "INTEGER"))
	     (read-byte-sequence s in :start start :end end))
	    ((or (string= set "CHARACTER")
		 (string= set "STRING-CHAR"))
	     (read-char-sequence s in :start start :end end))
	    (t
	     (error "READ-SEQUENCE: Did not understand stream-element-type ~a.~%" set))))))
	       

(defmacro with-standard-io-syntax (&body body)
  (let ((l '((*package*                    (find-package :common-lisp-user))
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

(defvar *wild-pathname-chars* '(#\* #\[ #\] #\{ #\}))

(defun wild-pathname-p (pathname &optional field-key)
  (labels ((wpp-string (p)
	     (loop for c in *wild-pathname-chars*
		do (loop for e in (coerce p 'list)
		      when (char= c e) do (return-from wpp-string t))))
	   (wpp-pathname (p)
	     (wpp-string (namestring p)))
	   )
    (cond ((stringp pathname)
	   (wpp-string pathname))
	  ((pathnamep pathname)
	   (case field-key
	     (:type
	      (wpp-string (pathname-type pathname)))
	     (:name
	      (wpp-string (pathname-name pathname)))
	     (:directory
	      (loop for d in (pathname-directory pathname)
		 when (wpp-string d) do (return-from wild-pathname-p t)))
	     (:host
	      (wpp-string (pathname-host pathname)))
	     (:device
	      (wpp-string (pathname-device pathname)))
	     (:version
	      (wpp-string (pathname-version pathname)))
	     ((nil)
	      (wpp-pathname pathname))
	     (otherwise
	      nil)))
	  (t nil))))


(lisp:clines
 "char probe_directory(char* dirname) {
  if(opendir(dirname)) return (char)1;
  else return (char)0;
  }")
(lisp:defentry probe_directory (string) (char probe_directory))

(defun probe-directory (pathname)
  "This is done in C code using the LIBC system call opendir."
  (let* ((p (cl-fad:pathname-as-directory pathname))
	 (s (namestring p)))
    (case (probe_directory s)
      (#\^@ nil)  ;; 0
      (#\^A p)    ;; 1
      (otherwise (error "probe-directory")))))

;; end of extensions.lisp 
