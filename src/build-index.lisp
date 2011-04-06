;; Copyright Leo Butler 2010
;; Released under the terms of GPLv3
;;
;; This code generates the hashs *info-section-hashtable* and
;; *info-deffn-defvr-hashtable*. These hashes contain look-up data used by
;; find-regex-match and other functions in cl-info.lisp.
;;
;; $Id: build-index.lisp,v 1.17 2011/01/31 18:44:35 work Exp work $

(in-package :cl-info)

;;
;; Maxima Info Files
;;
(defvar *maxima-info-list* nil
  "List of master info files. If set to nil, the function
  `canonicalize-info-pathnames' will use `*maxima-info-default*' and
  `*maxima-lang-subdir' to set this to a reasonable default. A
  reasonable value might be '(\"de.utf8/maxima.info\"
  \"en/maxima.info\" \"es.utf8/maxima.info\"), to pick up
  documentation in either the PWD or `*maxima-infodir*'.")
(defvar *maxima-info-default* "maxima.info"
  "Default main info file name.")
(defvar *maxima-info-index-list* '()
  "List of maxima-index.lisp files known to Maxima.")

(defun maxima-info-list-re (&optional (maxima-info-list *maxima-info-list*) (maxima-info-default *maxima-info-default*))
  (loop for f in (or maxima-info-list (list maxima-info-default))
     collect (concatenate 'string
			  (file-namestring (pathname f)) 
			  "-[0-9]+")))
(defvar *maxima-info-list-re* (maxima-info-list-re)
  "List of Regexps to identify each info file in the master info file.")
(defvar *maxima-info-default-re* (car *maxima-info-list-re*)
  "Default Regexp to identify each info file.")

;;
;; Info Nodes:
;;
;;Info files are broken into nodes arranged in hierarchical way, and
;;numbered. We want those nodes that are below the section nodes,
;;because in the Maxima info manual, section nodes are just ToCs.
;;
(defvar *info-nodes-start-header-re* "\\nFile: .+\\n\\n")
(defvar *info-nodes-start-re* (concatenate 'string
						 *info-nodes-start-header-re*
						 "([0-9]+\\.[0-9]+)"))
(defvar *info-nodes-name-re* "Node: ([^,]+)")
(defvar *info-nodes-end-re* (coerce '( #+gcl #\^_ #-gcl #\Us
				      #\| #\$
				      #-gcl #\|
				      #-gcl #\Null ) 'string)) ;; |$|Null
(defvar *info-nodes-hash-size* 200)

;;
;; Info Topics:
;;
;;We also want all topics. These begin with two newlines and a ' -- '
;;and they end with one of: the same, a new section/node or end of file.
;;
(defvar *info-topics-start-re* "\\n\\n -- ([^:]+): (\\S+)")
(defvar *info-topics-name-re*     "\\n -- ([^:]+): (\\S+)")
(defvar *info-topics-end-re* (concatenate 'string
					  *info-nodes-end-re*
					  "|\\n\\n[0-9]|\\n\\n --"))

(defvar *info-topics-hash-size* 2000)


;;
;; Hashes
;;
(defvar *info-section-hashtable* (make-hash-table :test #'eql :size 500)
  "Hashtable whose keys are the info-nodes-names identified by
  `*info-nodes-name-re*' and whose values are a list: '(filename
  character-offset length). Additional information may be appended to this
  list, but is not used at present.")
(defvar *info-deffn-defvr-hashtable* (make-hash-table :test #'eql :size 10000)
  "Hashtable whose keys are the info-topics-names identified by
  `*info-topics-name-re*' and whose values are a list: '(filename
  character-offset length). Additional information may be appended to this
  list, but is not used at present.")
(defvar *info-files* (make-hash-table :test #'equal)
  "Hash table of info files: key=filename, value=dotted list, whose
  car is the symbol 'string and cdr is the file's contents.")

(defvar *info-debug-bi* nil)
(defmacro -info (&rest args)
  (declare (ignorable args))
  (let* ((names+args (loop for a in args
			when (symbolp a) collect `(format t "~&~a: " ',a)
			collect `(format t "~a" ,a))))
    `(let ()
       (declare (special *info-debug-bi*))
       (if *info-debug-bi*
	   (progn
	     ,@names+args
	     (format t "~%~%"))))))
(defmacro +info (&rest args)
  `(-info ,@args))

;;
;; Helper functions + macros
;;
(declaim (special maxima:*maxima-infodir* maxima:*maxima-lang-subdir*))
(defun canonicalize-info-pathnames (p)
  (cond ((null p)
	 (list (canonicalize-info-pathnames *maxima-info-default*)))
	((consp p)
	 (cons (canonicalize-info-pathnames (car p))
	       (and (cdr p)
		    (canonicalize-info-pathnames (cdr p)))))
	((pathnamep p)
	 p)
	((stringp p)
	 (setq p (or (file-exists-p p)
		     (make-pathname :directory (pathname-directory
						(concatenate 'string
							     maxima:*maxima-infodir*
							     (and maxima:*maxima-lang-subdir*
								  (concatenate 'string "/" maxima:*maxima-lang-subdir*))
							     "/maxima.info")) ; "/maxima.info" part is not used
				    :name (pathname-name (pathname p))
				    :type (pathname-type (pathname p)))))
	 p)
	(t
	 nil)))

(defmacro setf-hash (to from &optional (delete-first nil))
  (let* ((k (gensym))
	 (v (gensym))
	 (over-write (if delete-first
			 `(maphash #'(lambda (,k ,v) (declare (ignore ,v)) (remhash ,k ,to)) ,to))))
    `(progn
       ,over-write
       (maphash #'(lambda (,k ,v) (setf (gethash ,k ,to) ,v)) ,from)
       t)))

;;
;; Core functions
;;
(defun get-info-file (&key
		      ((:over-write over-write) t)
		      ((:maxima-info maxima-info) nil)
		      ((:maxima-info-re maxima-info-re) *maxima-info-default-re*))
  "Slurps a main info file and extracts the sub info file names. If
`over-write' is true, then all keys are removed from `*info-files*'
before adding new contents."
  (flet ((get-info-file-pathnames ()
	   (let ((info-dir (pathname-directory maxima-info))
		 (str-contents (slurp maxima-info)))
	     (loop for f in (all-matches-as-strings maxima-info-re str-contents)
		for fp = (pathname f)
		for fn = (pathname-name fp)
		for ft = (pathname-type fp)
		for file = (make-pathname :directory info-dir :name fn :type ft)
		when (file-exists-p file) collect file))))
    (let ((info-files (make-hash-table :test #'equal))
	  (filenames (get-info-file-pathnames)))
	  (loop for filename in filenames
	     for str-contents = (slurp filename)
	     for k = (namestring filename)
	     for v = `(nil . ,str-contents)
	     do (setf (gethash k info-files) v))
	  (if over-write
	      (setf-hash *info-files* info-files))
	  info-files
	  )))

(defun periodically-extend (l n)
  (let ((k (if (= 1 (length l)) (1- n) (floor (/ n (length l)))))
	(m (copy-list l)))
    (dotimes (i k m)
      (setq m (append m (copy-list l))))))

(defun get-all-info-files (&key
			   ((:over-write over-write) t)
			   ((:maxima-info-list maxima-info-list) *maxima-info-list*)
			   ((:maxima-info-list-re maxima-info-list-re) *maxima-info-list-re*))
  "Loops over a joint list of main info files and regexps, calling `get-info-file' on each."
  (if (< (length maxima-info-list-re) (length maxima-info-list))
      (setq maxima-info-list-re (periodically-extend maxima-info-list-re (length maxima-info-list))))
  (loop for maxima-info in maxima-info-list
     for maxima-info-re in maxima-info-list-re
     do (get-info-file :maxima-info maxima-info :maxima-info-re maxima-info-re :over-write over-write)))

(defun get-info-file-string-contents (filename &key
				      ((:info-files info-files) *info-files*)
				      ((:start start) 0)
				      ((:end end) nil)
				      ((:length length) nil))
  "Extracts the contents, from character `start' to `end' (equal to start+length), from `filename'."
  (setq end (if length (+ start length) end))
  (setq filename (if (pathnamep filename) (namestring filename) filename))
  (let* ((slurp (null (cdr (gethash filename info-files))))
	 (str-contents (if slurp
			   (slurp filename)
			   (cdr (gethash filename info-files)))))
    (if slurp (setf (gethash filename info-files) `(nil . ,str-contents)))
    (subseq str-contents start end)))
  
(defun hash-keys (h)
  (if (hash-table-p h)
      (loop for k being the hash-keys of h
	 collect k)))

(defun get-nodes+topics (&key
			 ((:over-write over-write) t)
			 ((:info-files info-files) *info-files*)
			 ((:info-nodes-start-re info-nodes-start-re) *info-nodes-start-re*)
			 ((:info-nodes-name-re info-nodes-name-re) *info-nodes-name-re*)
			 ((:info-nodes-end-re info-nodes-end-re) *info-nodes-end-re*)
			 ((:info-nodes-hash-size info-nodes-hash-size) *info-nodes-hash-size*)
			 ((:info-topics-start-re info-topics-start-re) *info-topics-start-re*)
			 ((:info-topics-name-re info-topics-name-re) *info-topics-name-re*)
			 ((:info-topics-end-re info-topics-end-re) *info-topics-end-re*)
			 ((:info-topics-hash-size info-topics-hash-size) *info-topics-hash-size*))
  "Creates the hash table `*info-section-hashtable*'."
  (let ((filenames (hash-keys info-files))
	(info-nodes-s-e  (make-hash-table :test #'eql :size info-nodes-hash-size))
	(info-topics-s-e (make-hash-table :test #'eql :size info-topics-hash-size))
	(topic-types     (make-hash-table :test #'equal))
	)
    (labels ((topic-types (tt)
	       (cond ((string= (gethash tt topic-types) tt)
		      (gethash tt topic-types))
		     (t
		      (setf (gethash tt topic-types) tt))))
	     (safe-setf-hash (new-k given-k h v)
	       (unless (string= new-k given-k)
		 (setf (gethash new-k h) v)))
	     (get-topics-in-text-block (contents filename k a b l info-node-name)
	       (+info k a b l)
		 (do-register-groups (topic-type topic-name) (info-topics-name-re contents nil :start a :end b :sharedp t)
		   (safe-setf-hash topic-name k info-topics-s-e (list filename a l info-node-name (topic-types topic-type)))))
	     (get-node-name (re contents b e)
	       (+info re b e)
	       (register-groups-bind (n-n) (re contents :start b :end e)
		 n-n))
	     )
      (macrolet ((begin-topic (topics)
		   `(if ,topics (car ,topics) -3))
		 (end-topic (topics e)
		   `(if (cadr ,topics) (cadr ,topics) ,e))
		 (get-node-begin-end ()
		   `(let (nodes e s)
		     (do-scans (b m-e r-s r-e info-nodes-start-re contents
				  (reverse nodes))
		       (setf e (scan info-nodes-end-re contents :start (1+ b)))
		       (setf s (aref r-s 0))
		       (+info b e s)
		       (push b nodes)
		       (push e nodes)
		       (push s nodes))))
		 (get-topics-begin-end ()
		   `(let (topics)
		      (+info info-topics-start-re b e s)
		      (do-scans (m-s m-e unused-a unused-b info-topics-start-re contents
				    (reverse topics) :start b :end e)
			(and m-e
			     (setf m-e (scan info-topics-end-re contents :start m-e :end e))
			     (push m-s topics)
			     (push m-e topics)))))
		 )
      (loop for filename in filenames
	 for contents = (get-info-file-string-contents filename)
	 for nodes = (get-node-begin-end)
	 do
	   (+info filename nodes)
	   (loop for (b e s) on nodes by #'cdddr
	      for info-node-name = (get-node-name info-nodes-name-re contents b e)
	      for l = (- e s)
	      for k = info-node-name
	      for v = (list filename s l)
	      for topics = (get-topics-begin-end)
	      when topics do
		(setf (gethash k info-nodes-s-e) v)
		(+info b e s k v topics)
		(do* ((topics   topics                 (cddr topics))
		      (bt      (begin-topic topics)    (begin-topic topics))
		      (et      (end-topic topics e)    (end-topic topics e))
		      (l       (- et bt)               (- et bt)))
		     ((< bt 0))
		  (+info bt et topics)
		  (do-register-groups (topic-type topic-name) (info-topics-start-re contents nil :start bt :end et :sharedp t)
		    (safe-setf-hash topic-name "" info-topics-s-e (list filename (+ 2 bt) (- l 2) info-node-name (topic-types topic-type)))
		    (+info topic-type topic-name info-node-name)
		    (get-topics-in-text-block contents filename topic-name bt et l info-node-name)))
		 )))
      (and over-write
	   (setf-hash *info-section-hashtable* info-nodes-s-e)
	   (setf-hash *info-deffn-defvr-hashtable* info-topics-s-e))
      (values info-nodes-s-e info-topics-s-e)
      )))

(defun setup-help-database (&key
			    ((:over-write over-write) t)
			    ((:maxima-info-list maxima-info-list) *maxima-info-list*)
			    ((:maxima-info-list-re maxima-info-list-re) *maxima-info-list-re*)
			    ((:info-files info-files) *info-files*)
			    ((:info-nodes-start-re info-nodes-start-re) *info-nodes-start-re*)
			    ((:info-nodes-name-re info-nodes-name-re) *info-nodes-name-re*)
			    ((:info-nodes-end-re info-nodes-end-re) *info-nodes-end-re*)
			    ((:info-nodes-hash-size info-nodes-hash-size) *info-nodes-hash-size*)
			    ((:info-topics-start-re info-topics-start-re) *info-topics-start-re*)
			    ((:info-topics-end-re info-topics-end-re) *info-topics-end-re*)
			    ((:info-topics-name-re info-topics-name-re) *info-topics-name-re*)
			    ((:info-topics-hash-size info-topics-hash-size) *info-topics-hash-size*))
  (if (null maxima-info-list)
      (setf maxima-info-list (canonicalize-info-pathnames nil)))
  (cond (over-write
	 (clrhash *info-deffn-defvr-hashtable*)
	 (clrhash *info-section-hashtable*))
	(t t))
  (+info maxima-info-list)
  (get-all-info-files :over-write over-write 
		      :maxima-info-list maxima-info-list 
		      :maxima-info-list-re maxima-info-list-re)
  (+info *info-files*)
  (get-nodes+topics :over-write over-write :info-files info-files
		    :info-nodes-start-re info-nodes-start-re 
		    :info-nodes-name-re info-nodes-name-re 
		    :info-nodes-end-re info-nodes-end-re 
		    :info-nodes-hash-size info-nodes-hash-size
		    :info-topics-start-re info-topics-start-re
		    :info-topics-name-re info-topics-name-re 
		    :info-topics-end-re info-topics-end-re 
		    :info-topics-hash-size info-topics-hash-size)
  )

;;
;; Utilities to dump info hashes.
;;

(defun print-info-hashes (&optional (file nil))
  (labels ((print-hash-table (h &optional (out *standard-output*))
	     (format out "~s" (loop for k being the hash-keys of h
				 for v being the hash-values of h
				 collect (cons k v))))
	   (dump (out x)
	     (format out "(defparameter ~s (make-hash-table :test #'eql))~%" x)
	     (format out "(loop for u in ~%'")
	     (print-hash-table (eval x) out)
	     (format out "~%do (setf (gethash (car u) ~a) (cdr u)))~%" x))
	   (dump-info-files (out x)
	     (format out ";;We deliberately zero-out the entries of *info-files*~%")
	     (format out "(defparameter ~s (make-hash-table :test #'equal))~%" x)
	     (let ((xev (eval x)))
	       (loop for k being the hash-keys of xev
		  for v = (car (gethash k xev))
		  do (format out "(setf (gethash ~s ~s) '())~%" k x))))
	   (dump-hashes (out)
	     (format out "(in-package :cl-info)~%")
	     (dump out '*info-deffn-defvr-hashtable*)
	     (dump out '*info-section-hashtable*)
	     (dump-info-files out '*info-files*)))
    (cond (file
	   (with-standard-io-syntax
	     (with-open-file (out file :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create
				  :element-type 'character
				  )
	       (dump-hashes out)))
	   (pushnew file *maxima-info-index-list*))
	  (t
	   (with-standard-io-syntax
	     (dump-hashes *standard-output*))))))

;;
;; PATCHED VERSIONS OF FNS IN cl-info.lisp
;;

(defun read-info-text (value)
  "Value is a 4-element list of (matched-string filename start length)
where start and length are the character offsets in the filename where the
docuemntation for the matched string is. Printing is done by
`get-info-file-string-contents'."
  (let* ((match (first value))
	 (filename (second value))
	 (start (third value))
	 (length (fourth value)))
    (declare (ignore match))
    ;;(format t "x: ~a~%file: ~a~%start ~a~%length: ~a~%" value filename start length)
    (get-info-file-string-contents filename :start start :length length)))

(defun find-regex-matches (regex-string hash)
  "Each key in `hash' is matched against `regex-string'. Matched keys
are consed with the hash's value and collected in a list. The list is
sorted (and generally passed to `read-info-text')."
  (let ((regex-matches
	 (loop for k being the hash-keys of hash
	    when (scan regex-string k) collect (cons k (gethash k hash)))))
    ;;(format t "~a~%" regex-matches)
    (stable-sort regex-matches #'string-lessp :key #'car)))

;; end of build-index.lisp
