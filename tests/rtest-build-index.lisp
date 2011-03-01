;; Copyright Leo Butler 2010
;; Released under the terms of GPLv3
;;
;; Reversion tests for build-info.lisp
;;
;; $Id: rtest-build-index.lisp,v 1.9 2011/01/28 01:52:17 work Exp work $

(in-package :cl-info)
(use-package :rtest)

(defparameter x (make-instance 'test :tname "build-index" :tpasses 0 :tfails nil :tnumber 0))

;;
;; Check helper functions/macros
;;
(tagbody
#+clisp (defrtest 'x)
#-clisp (defrtest 'x :func #'file-exists-p :inputs '(".") :equality-fn #'(lambda(x y)(and x y)))
(defrtest 'x :func #'file-exists-p :inputs '("/dev/null") :answer (probe-file '#P"/dev/null"))

;; canonicalize-info-pathnames
(let* ((default-answer (canonicalize-info-pathnames *maxima-info-default*))
       (pwd (maxima::maxima-getenv "PWD"))
       (rel-dirs '("rtest-build-index/en/maxima.info" "rtest-build-index/de.utf8/maxima.info" "rtest-build-index/es.utf8/maxima.info"))
       (abs-dirs (mapcar #'(lambda (f) (concatenate 'string pwd "/" f)) rel-dirs)))
  (labels ((equal-pathname-p (x y)
	     (cond ((and (or (stringp x) (pathnamep x))
			 (or (stringp y) (pathnamep y)))
		    (equal (probe-file x) (probe-file y)))
		   ((and (consp x) (consp y))
		    (cons (equal-pathname-p (car x) (car y)) (equal-pathname-p (cdr x) (cdr y))))
		   ((not (and x y))
		    t)
		   (t
		    (error "equal-pathname-p"))))
	   (def-rtest (x y)
	     (let* ((args (loop for (a b) on y by #'cddr collect a collect `(quote ,b)))
		    (expr `(defrtest x :func #'canonicalize-info-pathnames ,@args)))
	       (format t "~s~%" expr)
	       (eval expr))))
    (loop for f in rel-dirs
       for p in abs-dirs
       do (defrtest 'x :func #'canonicalize-info-pathnames :inputs (list f) :answer p :equality-fn #'equal-pathname-p))
    (loop for args in (list (list :inputs '(nil)               :answer `(,default-answer))
			    (list :inputs '((nil nil))         :answer `((,default-answer) (,default-answer)))
			    (list :inputs '((nil (nil)))       :answer `((,default-answer) ((,default-answer))))
			    (list :inputs `(,default-answer)   :answer default-answer)
			    (list :inputs `((,default-answer)) :answer `(,default-answer))
			    (list :inputs `("maxima.info")     :answer default-answer))
       for arg-list = (append (list 'x :func #'canonicalize-info-pathnames) args)
       do
	 `(defrtest ,@arg-list))
    ))


;; setf-hash
(let ((h1 (make-hash-table :test #'eql))
      (h2 (make-hash-table :test #'eql)))
  (loop for i from 0 to 9
     do
       (setf (gethash i h1) i)
       (setf (gethash i h2) (* 2 i)))
  (setf-hash h1 h2)
  (defrtest 'x :func #'(lambda() (equalp h1 h2)) :inputs '() :answer t :equality-fn #'eq)
  )

;; get-info-file-names
(defun file= (x y)
  (string= (pathname-name x) (pathname-name y)))
(defun file<= (x y)
  (string<= (pathname-name x) (pathname-name y)))
(defun file-list= (x y)
  (not (some #'null (mapcar #'file= x y))))
(defun sorted-directory (dir)
  (stable-sort (directory dir) #'file<=))

(let ((files (sorted-directory "rtest-build-index/en/maxima.info-*")))
  (defrtest 'x :func #'(lambda ()
			 (let* ((maxima-info (probe-file "rtest-build-index/en/maxima.info"))
				(info-dir (pathname-directory maxima-info))
				(maxima-info-re *maxima-info-default-re*))
			   (get-info-file-names+external-format maxima-info info-dir maxima-info-re)))
	    :answer `(,@files)
	    :equality-fn #'(lambda(x y) (not (some #'null (mapcar #'file= x y))))))

;; slurp-info-files
(defrtest 'x :func #'(lambda ()
		       (labels ((random-lower-case ()
				  (+ 97 (random 25)))
				(random-file-name (l &optional (ending ""))
				  (do* ((k 0   (1+ k))
					(c nil (random-lower-case))
					(s nil (cons c s)))
				       ((>= k l)
					(concatenate 'string (map 'string #'code-char s) ending))))
				(weak-char= (a b)
				  (or (char= a b)
				      #+clisp (or (and (char= a #\Return) (char= b #\Newline)) (and (char= b #\Return) (char= a #\Newline)))
				      ))
				(weak-string= (x y)
				  (not (some #'null (mapcar #'weak-char= (coerce x 'list) (coerce y 'list)))))
				(write-read (ef n)
				  (let ((write-chars (do* ((k -1    (1+ k))
							   (c #\Nul (code-char k))
							   (s ""    (format nil "~3s ~5a ~s~%" k c c))
							   (str ""  (concatenate 'string str s)))
							  ((>= k n) str)))
					(file-name (make-pathname :directory '(:absolute "tmp") :name (random-file-name 10 ".txt")))
					(read-chars))
				    (progn
				      (with-open-file (out file-name :direction :output :if-exists :overwrite :if-does-not-exist :create :element-type 'character :external-format ef)
					(format out "~a" write-chars))
				      (setq read-chars (slurp-info-file file-name ef))
				      (if (probe-file file-name) (delete-file file-name))
				      (weak-string= read-chars write-chars)))))
			 (let ((utf8 #+clisp 'charset:utf-8
				     #-clisp ':utf-8)
			       (iso88591 #+clisp 'charset:iso-8859-1
					 #-clisp ':iso-8859-1))
			   (and (write-read utf8 127) (write-read iso88591 255)))))
	  :answer t)

(let ((*info-section-hashtable*     (make-hash-table :test #'eql :size 600))
      (*info-deffn-defvr-hashtable* (make-hash-table :test #'eql :size 6000)))
  (macrolet ((lambda-lex-env (&rest body)
	       `(lambda ()
		  (let* ((*maxima-info-list* (canonicalize-info-pathnames '("rtest-build-index/de.utf8/maxima.info" "rtest-build-index/en/maxima.info" "rtest-build-index/es.utf8/maxima.info")))
			 (*maxima-info-list-re* (maxima-info-list-re))
			 (*info-files* (make-hash-table :test #'equal))
			 (maxima-info (car *maxima-info-list*)))
		    (declare (ignorable *maxima-info-list* *maxima-info-list-re* *info-files* maxima-info))
		    ,@body))))
    ;; get-info-file
    (defrtest 'x :func (lambda-lex-env
			(file-list= (sorted-directory "rtest-build-index/de.utf8/maxima.info-*")
				    (stable-sort (mapcar #'probe-file (hash-keys (get-info-file :maxima-info maxima-info :over-write nil))) #'file<=))))
    (defrtest 'x :func (lambda-lex-env
			(let* ((info-file (get-info-file :maxima-info maxima-info :over-write nil))
			       (file-name (car (hash-keys info-file)))
			       (read-length (length (cdr (gethash file-name info-file))))
			       (file-length (with-open-file (f file-name :element-type 'unsigned-byte) (file-length f))))
			  (eql file-length read-length))))
    ;; get-all-info-files
    (defrtest 'x :func (lambda-lex-env
			(get-all-info-files)
			(file-list= (sorted-directory "rtest-build-index/*/maxima.info-*")
				    (stable-sort (mapcar #'probe-file (hash-keys *info-files*)) #'file<=))))
    ;; setup-help-database
    (defrtest 'x :func (lambda-lex-env
			(setup-help-database)
			(macrolet ((expand-match (e)
				     (with-output-to-string (stream nil :element-type 'character)
				       (dolist (c e)
					 (princ c stream)))))
			  (labels ((sort-on-second (list)
				     (stable-sort list #'(lambda (a b) (<= (cadr a) (cadr b)))))
				   (get-matches (regex h)
				     (sort-on-second
				      (mapcar #'(lambda (r) (list (first r) (third r) (fourth r) (fifth r)))
					      (find-regex-matches regex h)))))
			    (let ((macro-matches    (get-matches "^Ma[ck]ros$" *info-section-hashtable*))
				  (macro-matches-e  (sort-on-second '(("Macros" 193000 8636 nil) ("Macros" 135070 9085 nil) ("Makros" 282126 8529 nil))))
				  (expand-matches   (get-matches "^expand$" *info-deffn-defvr-hashtable*))
				  (expand-matches-e (sort-on-second `(("expand" 288341 4373 ,(expand-match (#\F #\u #\n #\k #\t #\i #\o #\n #\e #\n #\  #\u #\n #\d #\  #\V #\a #\r #\i
														#\a #\b #\l #\e #\n #\  #\f #\LATIN_CAPITAL_LETTER_A_WITH_TILDE
														#\VULGAR_FRACTION_ONE_QUARTER #\r #\  #\d #\i #\e #\  #\V #\e #\r #\e #\i #\n
														#\f #\a #\c #\h #\u #\n #\g)))
								      ("expand" 238810 4314 "Functions and Variables for Simplification")
								      ("expand" 253740 4495 ,(expand-match (#\F #\u #\n #\c #\i #\o #\n #\e #\s #\  #\y #\  #\v #\a #\r #\i #\a #\b #\l
														#\e #\s #\  #\p #\a #\r #\a #\  #\s #\i #\m #\p #\l #\i #\f #\i #\c #\a #\c
														#\i #\LATIN_CAPITAL_LETTER_A_WITH_TILDE #\SUPERSCRIPT_THREE #\n)))))))
			      ;;(format t "~&~a~%~a~%~a~%~a~%" macro-matches macro-matches-e expand-matches expand-matches-e)
			      (and (equal macro-matches macro-matches-e)
				   (equal expand-matches expand-matches-e)))))))
    ;; print-info-hashes
    (defrtest 'x :func (lambda-lex-env
			(labels ((read-write-info-hashes (s)
				   (let ((*info-deffn-defvr-hashtable* *info-deffn-defvr-hashtable*)
					 (*info-section-hashtable* *info-section-hashtable*)
					 s-out-s s-in)
				     (print-info-hashes)
				     (setq s-out-s (get-output-stream-string s))
				     (setq s-in (make-string-input-stream s-out-s))
				     (load s-in)
				     (values *info-deffn-defvr-hashtable* *info-section-hashtable*)))
				 (sort-on-second (list)
				   (stable-sort list #'(lambda (a b) (<= (cadr a) (cadr b)))))
				 (copy-eql-to-equal-hashtable (in)
				   (let ((out (make-hash-table :test #'equal :size (hash-table-size in))))
				     (loop for k being the hash-keys of in
					for v = (gethash k in)
					do (setf (gethash k out) (push v (gethash k out))))
				     (loop for k being the hash-keys of out
					do (setf (gethash k out) (sort-on-second (gethash k out))))
				     out))
				 (hash-table-equalp (hin1 hin2 &optional (fn #'list))
				   (let ((h1 (copy-eql-to-equal-hashtable hin1))
					 (h2 (copy-eql-to-equal-hashtable hin2)))
				     (let ((d12 (loop for k1 being the hash-keys of h1
						   for v1 = (gethash k1 h1)
						   for v2 = (gethash k1 h2)
						   when (null (equalp v1 v2)) collect (list k1 v1 v2)))
					   (d21 (loop for k2 being the hash-keys of h2
						   for v1 = (gethash k2 h1)
						   for v2 = (gethash k2 h2)
						   when (null (equalp v1 v2)) collect (list k2 v1 v2))))
				       (funcall fn d12 d21)))))
			  (let* ((s-out (make-string-output-stream :element-type 'character))
				 (*standard-output* s-out)
				 h11 h21 h12 h22)
			    (multiple-value-setq (h11 h21) (read-write-info-hashes s-out))
			    (let ((*info-deffn-defvr-hashtable* h11)
				  (*info-section-hashtable* h21))
			      (multiple-value-setq (h12 h22) (read-write-info-hashes s-out)))
			    (list (hash-table-equalp h11 h12)
				  (hash-table-equalp h21 h22)))))
	      :answer '((nil nil) (nil nil)) ;; 
	      )))

;; external-formats
(defrtest x :func (lambda ()
		    (flet ((get-set-external-format (answer input)
			     (eql (get-external-format-name (set-external-format input)) answer)))
		      (and (get-set-external-format :utf-8 "utf8")
			   (get-set-external-format :utf-8 "utf-8")
			   (get-set-external-format :iso-8859-1 "iso88591")
			   (get-set-external-format :iso-8859-1 "iso8859-1")
			   (get-set-external-format :iso-8859-1 "iso-88591")
			   (get-set-external-format :iso-8859-1 "iso-8859-1")
			   #-cmu (and (get-set-external-format :windows-1251 "windows1251") (get-set-external-format :windows-1251 "windows-1251"))
			   ))))

:check-report
(do-tests x #'check)
(report-summary x)
)
;; end of rtest-build-index.lisp
