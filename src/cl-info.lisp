(in-package :cl-info)

;; These 4 are defined in build-index.lisp
(defvar *maxima-info-index-list*)
(defvar *info-deffn-defvr-hashtable*)
(defvar *info-section-hashtable*)
(defvar *info-files*)

(defvar *info-case-fold-search* nil
  "If t, info searches are done case insensitively.")
(defvar *info-special-chars* '(#\. #\? #\+ #\* #\[ #\] #\{ #\} #\| #\^)
  "Single characters that are escaped when info-exact-match is called.")
(defvar *prompt-prefix* "")
(defvar *prompt-suffix* "")

(defun print-prompt (prompt-count)
  (format t "~&~a~a~a"
	  *prompt-prefix*
	  (if (zerop prompt-count)
	      (intl:gettext "Enter space-separated numbers, a range `M-N', `all' or `none': ")
	      (intl:gettext "Still waiting: "))
	  *prompt-suffix*))

(defvar +select-by-keyword-alist+
  '((noop "") (all "a" "al" "all") (none "n" "no" "non" "none")))

(defun parse-user-choice (nitems)
  (let ((line (read-line))
	keyword nth list)
    (flet ((safe-push (i)
	     (cond ((>= i nitems)
		    (format *debug-io* (intl:gettext "~&Discarding invalid number ~d.") nth))
		   (t
		    (push i list)))))
      (do-register-groups (b s e) ("((\\d+)-(\\d+)|(\\d+))" line)
	(cond (s
	       (loop for i from (parse-integer s) to (parse-integer e) do (safe-push i)))
	      (b
	       (safe-push (parse-integer b)))))
      (setq keyword (or
		     (car (rassoc
			   (string-right-trim
			    '(#\space #\tab #\newline #\;) (subseq line 0))
			   +select-by-keyword-alist+
			   :test #'(lambda (item list)
				     (member item list :test #'string-equal))))
		     'noop))
      (cons keyword (reverse list)))))

(defun select-info-items (selection items)
  (case (pop selection)
    (noop (loop
	   for i in selection
	   collect (nth i items)))
    (all items)
    (none 'none)))

; ------------------------------------------------------------------
; STUFF ABOVE SALVAGED FROM PREVIOUS INCARNATION OF SRC/CL-INFO.LISP
; STUFF BELOW IS NEW, BASED ON LOOKUP TABLE BUILT AHEAD OF TIME
; ------------------------------------------------------------------

; ------------------ search help topics ------------------

(defun info-exact (x)
  (let ((exact-matches (exact-topic-match x)))
    (if (null exact-matches)
      (progn
        (format t (intl:gettext "  No exact match found for topic `~a'.~%  Try `?? ~a' (inexact match) instead.~%~%") x x)
        nil)
      (progn
        (format t "~%")
        (loop for item in exact-matches
              do (format t "~A~%~%" (read-info-text item)))
        (if (some-inexact x (inexact-topic-match x))
          (format t "  There are also some inexact matches for `~a'.~%  Try `?? ~a' to see them.~%~%" x x))
        t))))

(defun some-inexact (x inexact-matches)
  (some #'(lambda (y) (not (equal y x))) (mapcar #'car inexact-matches)))

(defun regex-sanitize (x)
  "x is a search string to be sanitized. The first n contiguous
special characters are escaped; subsequent special characters are NOT
escaped. The list of special characters is `*info-special-char*'."
  (let ((match t) e y (x (coerce x 'list)))
    (flet ((is-a-special-char (s)
	     (some (lambda(c) (string= c s)) *info-special-chars*))
	   (escape-char (c)
	     (list c #\\)))
       (loop for s in x
	  do
	    (setq match (and match (is-a-special-char s)))
	    (setq  e (if match (escape-char s) (list s)))
	    (setq y (append e y))))
    (coerce (reverse y) 'string)))
      
(defun exact-topic-match (topic)
  (check-info-hashes)
  (setq topic (regex-sanitize topic))
  (setq topic (if *info-case-fold-search*
		  (concatenate 'string "^(?i:" topic ")$")
		  (concatenate 'string "^("    topic ")$")))
  (reverse
   (append
    (find-regex-matches topic *info-deffn-defvr-hashtable*)
    (find-regex-matches topic *info-section-hashtable*))))

(defun info (x)
  (let (wanted tem)
    (setf tem (inexact-topic-match x))
    (when tem
      (let ((nitems (length tem)))
	(when (> nitems 1)
	  (format t "~&#~4T~a~30T~a" (intl:gettext "Topic") (intl:gettext "Description")))
        (loop for i from 0 for item in tem do
          (when (> nitems 1)
            (let ((heading-title (nth 3 (cdr item))))
              (format t "~&~d:~4,2T~a~@[~30,5T(~a)~]"
                      i
                      (car item)
                      heading-title))))

        (setq wanted
              (if (> nitems 1)
              (loop
               for prompt-count from 0
               thereis (progn
                     (finish-output *debug-io*)
                     (print-prompt prompt-count)
                     (force-output)
                     (clear-input)
                     (select-info-items
                      (parse-user-choice nitems) tem)))
              tem))
        (clear-input)
        (finish-output *debug-io*)
        (when (consp wanted)
          (format t "~%")
          (loop for item in wanted
            do (format t "~A~%~%" (read-info-text item))))))

    (not (null tem))))

(defun inexact-topic-match (topic)
  (check-info-hashes)
  (setq topic (regex-sanitize topic))
  (setq topic (if *info-case-fold-search*
		  (concatenate 'string "(?i:" topic ")")
		  topic))
  (reverse
   (append
    (find-regex-matches topic *info-section-hashtable*)
    (find-regex-matches topic *info-deffn-defvr-hashtable*))))

(defun check-info-hashes ()
  (declare (special *maxima-info-index-list*))
  (cond ((or (null *info-section-hashtable*) (null *info-deffn-defvr-hashtable*) (null *info-files*)
	     (eq 0 (hash-table-count *info-section-hashtable*)) (eq 0 (hash-table-count *info-deffn-defvr-hashtable*)) (eq 0 (hash-table-count *info-files*)))
	 (flet ((setup-help-database ()
		  (format t "~&~a" (intl:gettext "Generating the help database, this may take a few seconds..."))
		  (setup-help-database)
		  (format t "~a~%" (intl:gettext "Done."))))
	   (cond (*maxima-info-index-list*
		  (loop for m in *maxima-info-index-list*
		     for maxima-info-index = (canonicalize-info-pathnames m)
		     do
		       (if (file-exists-p maxima-info-index)
			   (load maxima-info-index)
			   (setup-help-database))))
		 (t
		  (setup-help-database)))))))
	    
;; end of cl-info.lisp
