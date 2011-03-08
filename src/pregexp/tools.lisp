(load "pregexp.lisp")
(in-package :pregexp)
(load "tester.lisp")

(defun codes-string (code-vec)
  (declare (type vector code-vec))
  (declare (optimize (speed 3) (safety 0)))
  (map 'string #'code-char code-vec))


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

(defparameter mi1 (slurp-info-file "../../tests/rtest-build-index/de.utf8/maxima.info-1" :utf-8))


(test
 (pregexp-match *info-encoding-re* mi1 (- (length mi1) 3000))
 ("
coding: utf-8" "utf-8")

 (pregexp-match-positions *info-encoding-re* mi1 (- (length mi1) 3000))
 ((306174 . 306188) (306183 . 306188))

)