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
(defrtest 'x :func #'file-exists-p :inputs '(".") :equality-fn #'(lambda(x y)(and x y)))
(defrtest 'x :func #'file-exists-p :inputs '("/dev/null") :answer '#P"/dev/null")

;; canonicalize-info-pathnames
(let* ((default-answer (canonicalize-info-pathnames *maxima-info-default*))
       (pwd (maxima::maxima-getenv "PWD"))
       (rel-dirs '("en/maxima.info" "de.utf8/maxima.info" "es.utf8/maxima.info"))
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
	 (apply #'defrtest arg-list))
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
(let ((files (directory "en/maxima.info-*")))
  (defrtest 'x :func #'(lambda ()
			 (let* ((maxima-info (probe-file "en/maxima.info"))
				(info-dir (pathname-directory maxima-info))
				(maxima-info-re *maxima-info-default-re*))
			   (get-info-file-names maxima-info info-dir maxima-info-re)))
	    :answer `(,@files)))

;; slurp-info-files
(defrtest 'x :func #'(lambda ()
		       (labels ((random-lower-case ()
				  (+ 97 (random 25)))
				(random-file-name (l &optional (ending ""))
				  (do* ((k 0   (1+ k))
					(c nil (random-lower-case))
					(s nil (cons c s)))
				       ((>= k l)
					(concatenate 'string (map 'string #'code-char s) ending)))))
			 (let ((ascii-chars (do* ((k -1    (1+ k))
						  (c #\Nul (code-char k))
						  (s ""    (format nil "~3s ~5a ~s~%" k c c))
						  (str ""  (concatenate 'string str s)))
						 ((>= k 255) str)))
			       (file-name (make-pathname :directory "/tmp/" :name (random-file-name 10 ".txt")))
			       (read-chars))
			   (progn
			     (with-open-file (out file-name :direction :output :if-exists :overwrite :if-does-not-exist :create)
			       (format out "~a" ascii-chars))
			     (setq read-chars (slurp-info-file file-name))
			     (if (probe-file file-name) (delete-file file-name))
			     (string= read-chars ascii-chars)))))
	  :answer t)

(let ((*info-section-hashtable*     (make-hash-table :test #'eql :size 600))
      (*info-deffn-defvr-hashtable* (make-hash-table :test #'eql :size 6000)))
  (macrolet ((lambda-lex-env (&rest body)
	       `(lambda ()
		  (let* ((*maxima-info-list* (canonicalize-info-pathnames '("de.utf8/maxima.info" "en/maxima.info" "es.utf8/maxima.info")))
			 (*maxima-info-list-re* (maxima-info-list-re))
			 (*info-files* (make-hash-table :test #'equal))
			 (maxima-info (car *maxima-info-list*)))
		    (declare (ignorable *maxima-info-list* *maxima-info-list-re* *info-files* maxima-info))
		    ,@body))))
    ;; get-info-file
    (defrtest 'x :func (lambda-lex-env
			(equal (directory "de.utf8/maxima.info-*") (mapcar #'probe-file (hash-keys (get-info-file :maxima-info maxima-info :over-write nil))))))
    (defrtest 'x :func (lambda-lex-env
			(let* ((info-file (get-info-file :maxima-info maxima-info :over-write nil))
			       (file-name (car (hash-keys info-file)))
			       (read-length (length (cdr (gethash file-name info-file))))
			       (file-length (with-open-file (f file-name :element-type 'unsigned-byte) (file-length f))))
			  (eql file-length read-length))))
    ;; get-all-info-files
    (defrtest 'x :func (lambda-lex-env
			(get-all-info-files)
			(equal (directory "*/maxima.info-*") (mapcar #'probe-file (hash-keys *info-files*)))))
    ;; setup-help-database
    (defrtest 'x :func (lambda-lex-env
			(setup-help-database)
			(let ((macro-matches    (mapcar #'(lambda (r) (list (first r) (third r) (fourth r)))
							(find-regex-matches "^Ma[ck]ros$" *info-section-hashtable*)))
			      (macro-matches-e '(("Macros" 193000 8636) ("Macros" 135070 9085) ("Makros" 282280 8583)))
			      (expand-matches   (mapcar #'(lambda (r) (list (first r) (third r) (fourth r) (fifth r)))
							(find-regex-matches "^expand$" *info-deffn-defvr-hashtable*)))
			      (expand-matches-e '(("expand" 290879 4411 "Funktionen und Variablen fÃ¼r die Vereinfachung") ("expand" 238810 4314 "Functions and Variables for Simplification") ("expand" 253740 4495 "Funciones y variables para simplificación"))))
			  (format t "~a~%~a~%~a~%~a~%" macro-matches macro-matches-e expand-matches expand-matches-e)
			  (and (equal macro-matches macro-matches-e)
			       (equal expand-matches expand-matches-e)))))
    ;; dump-info-hashes
    (defrtest 'x :func (lambda-lex-env
			(let* ((s-out (make-string-output-stream :element-type 'character))
			       (*standard-output* s-out)
			       (s-in)
			       (s1)
			       (s2))
			  (dump-info-hashes)
			  (setq s1 (get-output-stream-string s-out))
			  (setq s-in (make-string-input-stream s1))
			  (load s-in)
			  (dump-info-hashes)
			  (setq s2 (get-output-stream-string s-out))
			  (string= s1 s2))))
    ))
(do-tests x #'check)
(report-summary x)

;; end of rtest-build-index.lisp
