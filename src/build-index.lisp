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
(defparameter *maxima-info-list* nil
  "List of master info files. If set to nil, the function
  `canonicalize-info-pathnames' will use `*maxima-info-default*' and
  `*maxima-lang-subdir' to set this to a reasonable default. A
  reasonable value might be '(\"de.utf8/maxima.info\"
  \"en/maxima.info\" \"es.utf8/maxima.info\"), to pick up
  documentation in either the PWD or `*maxima-infodir*'.")
(defparameter *maxima-info-default* "maxima.info"
  "Default main info file name.")

(defun maxima-info-list-re (&optional (maxima-info-list *maxima-info-list*) (maxima-info-default *maxima-info-default*))
  (loop for f in (or maxima-info-list (list maxima-info-default))
     collect (concatenate 'string
			  (file-namestring (pathname f)) 
			  "-[0-9]+")))
(defparameter *maxima-info-list-re* (maxima-info-list-re)
  "List of Regexps to identify each info file in the master info file.")
(defparameter *maxima-info-default-re* (car *maxima-info-list-re*)
  "Default Regexp to identify each info file.")

;;
;; Info Nodes:
;;
;;Info files are broken into nodes arranged in hierarchical way, and
;;numbered. We want those nodes that are below the section nodes,
;;because in the Maxima info manual, section nodes are just ToCs.
;;
(defparameter *info-nodes-start-header-re* "\\nFile: .+\\n\\n")
(defparameter *info-nodes-start-re* "[0-9]+\\.[0-9]+")
(defparameter *info-nodes-name-re* "(?<=Node: )([^,]+)")
(defparameter *info-nodes-end-re* "(?=(?:|$))")
(defparameter *info-nodes-re* (concatenate 'string
					   *info-nodes-start-header-re*
					   *info-nodes-start-re*
					   "(?:(?sm:.*?))"
					   *info-nodes-end-re*)
  "Regexp identifies the whole info-node, beginning at the header.")
(defparameter *info-nodes-hash-size* 200)

;;
;; Info Topics:
;;
;;We also want all topics. These begin with two newlines and a ' -- '
;;and they end with one of: the same, a new section/node or end of file.
;;
(defparameter *info-topics-start-re* "(?sm:^ -- )(.+?): ([^\\s]+)")
(defparameter *info-topics-name-re* ": ([^\\s]+)")
(defparameter *info-topics-end-re* "(?=(?:\\n\\n -- ||^[0-9]|$))")
(defparameter *info-topics-re* (concatenate 'string
					    *info-topics-start-re*
					    "(?:(?sm:.+?))"
					    *info-topics-end-re*)
  "Regexp identifies the whole topic listing, beginning two lines
  above the start.")
(defparameter *info-topics-hash-size* 2000)


;;
;; Hashes
;;
(defparameter *info-section-hashtable* (make-hash-table :test #'eql :size 500)
  "Hashtable whose keys are the info-nodes-names identified by
  `*info-nodes-name-re*' and whose values are a list: '(filename
  character-offset length). Additional information may be appended to this
  list, but is not used at present.")
(defparameter *info-deffn-defvr-hashtable* (make-hash-table :test #'eql :size 10000)
  "Hashtable whose keys are the info-topics-names identified by
  `*info-topics-name-re*' and whose values are a list: '(filename
  character-offset length). Additional information may be appended to this
  list, but is not used at present.")
(defparameter *info-files* (make-hash-table :test #'equal)
  "Hash table of info files: key=filename, value=dotted list, whose
  car is the symbol 'string and cdr is the file's contents.")

(defparameter *info-encoding-re* "\\ncoding: ([a-zA-Z0-9-]+)"
  "Regex to extract the encoding string in each master info file.")

;;
;; Helper functions + macros
;;
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
	 (setq p (or (probe-file p)
		     (make-pathname :directory (pathname-directory
						(concatenate 'string
							     maxima::*maxima-infodir*
							     (and maxima::*maxima-lang-subdir*
								  (concatenate 'string "/" maxima::*maxima-lang-subdir*))
							     "/maxima.info")) ; "/maxima.info" part is not used
				    :name (pathname-name (pathname p))
				    :type (pathname-type (pathname p)))))
	 p)
	(t
	 nil)))

;; Using  (maxima::$file_search (namestring f)) instead of probe-file
;; is *very* slow
(defun file-exists-p (f &optional (use-file-search nil))
  (cond (use-file-search
	 (maxima::$file_search (namestring f)))
	(t
	 (probe-file f))))

(defun codes-string (code-vec)
  (declare (type vector code-vec))
  (declare (optimize (speed 3) (safety 0)))
  (map 'string #'code-char code-vec))

(defmacro setf-hash (to from &optional (delete-first nil))
  (let* ((k (gensym))
	 (v (gensym))
	 (over-write (if delete-first
			 `(maphash #'(lambda (,k ,v) (declare (ignore ,v)) (remhash ,k ,to)) ,to))))
    `(progn
       ,over-write
       (maphash #'(lambda (,k ,v) (setf (gethash ,k ,to) ,v)) ,from)
       t)))

(defun get-info-file-names+external-format (maxima-info info-dir maxima-info-re &optional ef (info-encoding-re *info-encoding-re*))
  "Returns a list of info file pathnames in the master info file
maxima-info and the encoding (external format) of these files."
  (let* ((str-contents (slurp-info-file maxima-info ef))
	 (efr (get-info-file-encoding str-contents info-encoding-re))
	 info-file-names)
    (if (null (eq ef efr))
	(setf str-contents (slurp-info-file maxima-info efr)))
    (setf info-file-names
	  (loop for f in (cl-ppcre:all-matches-as-strings maxima-info-re str-contents)
       for fp = (pathname f)
       for fn = (pathname-name fp)
       for ft = (pathname-type fp)
       for file = (make-pathname :directory info-dir :name fn :type ft)
       when (file-exists-p file) collect file))
    (values info-file-names efr)))

(defun slurp-info-file (filename &optional ef)
  (cond (ef
	 (let ((fs))
	   (with-open-file (in filename :direction :input :element-type 'character :external-format ef)
	     (setq fs (file-length in))
	     (let ((contents (make-array fs :element-type 'character :initial-element #\Null)))
	       (with-standard-io-syntax
		 (read-sequence contents in :start 0 :end nil))
	       (coerce contents 'string)))))
	(t
	 (let ((fs))
	   (with-open-file (in filename :direction :input :element-type 'unsigned-byte)
	     (setq fs (file-length in))
	     (let ((contents (make-array fs :element-type 'unsigned-byte)))
	       (with-standard-io-syntax
		 (read-sequence contents in :start 0 :end nil))
	       (codes-string contents)))))))

(declaim (inline set-string-case))
(defun set-string-case (string)
  #+scl(setf string (if (eq ext:*case-mode* :upper) (string-upcase string) (string-downcase string)))
  #-scl(string-upcase string)
  )
(defun set-external-format (ef)
  (flet ((sanitize-external-format (ef-str)
	   (let (match match-l)
	     (multiple-value-setq (match match-l) (cl-ppcre:scan-to-strings "([a-zA-Z]+)-?([0-9]{1,4})-?(1)?" ef-str))
	     (if match
		 (set-string-case
		  (concatenate 'string
			       (aref match-l 0)
			       "-"
			       (aref match-l 1)
			       (if (aref match-l 2) "-1" "")))))))
    (let ((ef (cond ((symbolp ef) (sanitize-external-format (symbol-name ef)))
		    ((stringp ef) (sanitize-external-format ef))
		    (t nil))))
      (if ef
	  #+clisp
	  (find-symbol ef :charset)
	  #+cmu
	  (stream::find-external-format (intern ef :keyword))
	  #+(or sbcl scl)
	  (intern ef :keyword)
	  #-(or clisp cmu sbcl scl)
	  ef
	  ))))

(defparameter *info-default-external-format*
  (set-external-format :utf-8))

(defun get-external-format-name (ef)
  (flet ((symbol-name-as-keyword (s)
	   (intern (cl-ppcre:scan-to-strings "[a-zA-Z0-9-]+$" (symbol-name s))
		   :keyword)))
    (symbol-name-as-keyword
     (if ef
	 #+cmu
	 (stream::ef-name ef)
	 #+(or sbcl clisp)
	 ef
	 #+t
	 ef
	 ))))

(defun get-info-file-encoding (maxima-info-contents &optional (info-encoding-re *info-encoding-re*))
  (if *info-default-external-format*
      (let (dummy coding)
	(multiple-value-setq (dummy coding) (cl-ppcre:scan-to-strings info-encoding-re maxima-info-contents))
	(if coding
	    (set-external-format (aref coding 0))
	    *info-default-external-format*))))

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
  (let ((info-files (make-hash-table :test #'equal))
	ef filenames
	(info-dir (pathname-directory maxima-info)))
    (multiple-value-setq (filenames ef) (get-info-file-names+external-format maxima-info info-dir maxima-info-re))
    (loop for filename in filenames
       for str-contents = (slurp-info-file filename ef)
       for k = (namestring filename)
       for v = `(,ef . ,str-contents)
       do (setf (gethash k info-files) v))
    (if over-write
	(setf-hash *info-files* info-files))
    info-files
    ))

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
	 (ef (or (car (gethash filename info-files)) *info-default-external-format*))
	 (str-contents (if slurp
			   (slurp-info-file filename ef)
			   (cdr (gethash filename info-files)))))
    (if slurp (setf (gethash filename info-files) `(,ef . ,str-contents)))
    (subseq str-contents start end)))
  
(defun hash-keys (h)
  (if (hash-table-p h)
      (loop for k being the hash-keys of h
	 collect k)))

(defun get-nodes+topics (&key
			 ((:over-write over-write) t)
			 ((:info-files info-files) *info-files*)
			 ((:info-nodes-start-re info-nodes-start-re) *info-nodes-start-re*)
			 ((:info-nodes-re info-nodes-re) *info-nodes-re*)
			 ((:info-nodes-name-re info-nodes-name-re) *info-nodes-name-re*)
			 ((:info-nodes-hash-size info-nodes-hash-size) *info-nodes-hash-size*)
			 ((:info-topics-re info-topics-re) *info-topics-re*)
			 ((:info-topics-start-re info-topics-start-re) *info-topics-start-re*)
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
	       (cond ((string= new-k given-k)
		      t)
		     (t
		      (setf (gethash new-k h) v))))
	     (get-topics-in-text-block (contents filename k a b l info-node-name)
		 (cl-ppcre:do-register-groups (topic-type topic-name) (info-topics-start-re contents nil :start a :end b :sharedp t)
		   (safe-setf-hash topic-name k info-topics-s-e (list filename a l info-node-name (topic-types topic-type)))))
	     )
      (loop for filename in filenames
	 for contents = (get-info-file-string-contents filename)
	 for nodes = (cl-ppcre:all-matches info-nodes-re contents)
	 do
	 ;;(format t "Reading ~a~%" filename)
	   (loop for (b end) on nodes by #'cddr
	      for e = (1- end)
	      for info-node-name = (cl-ppcre:scan-to-strings info-nodes-name-re contents :start b)
	      for s = (cl-ppcre:scan info-nodes-start-re contents :start b)
	      for l = (- e s)
	      for k = info-node-name
	      for v = (list filename s l)
	      for topics = (cl-ppcre:all-matches info-topics-re contents :start b :end end)
	      do
		(if topics (setf (gethash k info-nodes-s-e) v))
		(loop for (bt et) on topics by #'cddr
		   for l = (- et bt)
		   do
		     (cl-ppcre:do-register-groups (topic-type topic-name) (info-topics-re contents nil :start bt :end et :sharedp t)
		       (safe-setf-hash topic-name "" info-topics-s-e (list filename bt l info-node-name (topic-types topic-type)))
		       (get-topics-in-text-block contents filename topic-name bt et l info-node-name)))))
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
			    ((:info-nodes-re info-nodes-re) *info-nodes-re*)
			    ((:info-nodes-name-re info-nodes-name-re) *info-nodes-name-re*)
			    ((:info-nodes-hash-size info-nodes-hash-size) *info-nodes-hash-size*)
			    ((:info-topics-re info-topics-re) *info-topics-re*)
			    ((:info-topics-start-re info-topics-start-re) *info-topics-start-re*)
			    ((:info-topics-hash-size info-topics-hash-size) *info-topics-hash-size*))
  (if (null maxima-info-list)
      (setf maxima-info-list (canonicalize-info-pathnames nil)))
  (cond (over-write
	 (clrhash *info-deffn-defvr-hashtable*)
	 (clrhash *info-section-hashtable*))
	(t t))
  (get-all-info-files :over-write over-write :maxima-info-list maxima-info-list :maxima-info-list-re maxima-info-list-re)
  (get-nodes+topics :over-write over-write :info-files info-files :info-nodes-start-re info-nodes-start-re :info-nodes-re info-nodes-re :info-nodes-name-re info-nodes-name-re :info-nodes-hash-size info-nodes-hash-size
		    :info-topics-start-re info-topics-start-re :info-topics-re info-topics-re :info-topics-hash-size info-topics-hash-size)
  )

;;
;; Utilities to dump info hashes.
;;

(defun print-info-hashes (&optional (file nil) (ef *info-default-external-format*))
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
		  for ef = (get-external-format-name v)
		  do (format out "(setf (gethash ~s ~s) '(#.(set-external-format '~s)))~%" k x ef))))
	   (dump-hashes (out)
	     (format out "(in-package :cl-info)~%")
	     (dump out '*info-deffn-defvr-hashtable*)
	     (dump out '*info-section-hashtable*)
	     (dump-info-files out '*info-files*))
	   (get-external-format (ef)
	     (let ((efs (remove-duplicates (loop for v being the hash-values of *info-files* collect (car v)))))
	       (cond ((> 1 (length efs))
		      (warn (intl:gettext "Info files have multiple external formats. Info database may be corrupted."))
		      (car ef))
		     ((null efs)
		      ef)
		     (t
		      (car efs))))))
    (cond (file
	   (setf ef (get-external-format ef))
	   (with-open-file (out file :direction :output
				:if-exists :supersede
				:if-does-not-exist :create
				:external-format ef)
	     (with-standard-io-syntax
	       (dump-hashes out))))
	  (t
	   (dump-hashes *standard-output*)))))

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
	    when (cl-ppcre:scan regex-string k) collect (cons k (gethash k hash)))))
    ;;(format t "~a~%" regex-matches)
    (stable-sort regex-matches #'string-lessp :key #'car)))

;; end of build-index.lisp
