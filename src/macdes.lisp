;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defvar $manual_demo "manual.demo")

(defmspec $example (l)
  (declare (special *need-prompt*))
  (let ((example (second l)))
    (when (symbolp example)
      ;; Coerce a symbol to be a string. Remove the first character,
      ;; it is a $-char.
      (setq example (coerce (cdr (exploden example)) 'string)))
    (unless (stringp example)
      (merror 
        (intl:gettext "example: argument must be a symbol or a string; found: ~M") example))
    ;; Downcase the string. $example is not case sensitive.
    (setq example (string-downcase example))
    (with-open-file (st ($file_search1 $manual_demo '((mlist) $file_search_demo)))
      (prog (tem all c-tag d-tag)
       again
       (setq tem (read-char st nil))
       (unless tem (go notfound))
       (unless (eql tem #\&) (go again))
       (setq tem (read-char st nil))
       (unless (eql tem #\&) (go again))
       ;; so we are just after having read &&

       (setq tem (read st nil nil))
       (unless tem (go notfound))
       ;; Coerce the topic in tem to be a string.
       (setq tem (coerce (exploden tem) 'string))
       (cond ((string= tem example)
	      (go doit))
	     (t (push tem all)
		(go again)))
       ;; at this stage we read maxima forms and print and eval
       ;; until a peek sees '&' as the first character of next expression,
       ;; but at first skip over whitespaces.
       doit       
       (when (member (setq tem (peek-char nil st nil)) 
                     '(#\tab #\space #\newline #\linefeed #\return #\page))
         ;; Found whitespace. Read char and look for next char.
         ;; The && label can be positioned anywhere before the next topic.
         (setq tem (read-char st nil))
         (go doit))
       (cond ((or (null tem) (eql tem #\&))
	      (setf *need-prompt* t)
	      (return '$done)))
       (setq tem (dbm-read st nil nil))
       (incf $linenum)
       (setq c-tag (makelabel $inchar))
       (unless $nolabels (setf (symbol-value c-tag) (nth 2 tem)))
       (let ($display2d)
	 (displa `((mlable) ,c-tag ,(nth 2 tem))))
       (setq $% (meval* (nth 2 tem)))
       (setq d-tag (makelabel $outchar))
       (unless $nolabels (setf (symbol-value d-tag) $%))
       (when (eq (caar tem) 'displayinput)
	 (displa `((mlable) ,d-tag ,$%)))
       (go doit)

       notfound
       (setf *need-prompt* t)
       (if (= (length l) 1)
         (return `((mlist) ,@(nreverse all)))
         (progn
           (mtell (intl:gettext "example: ~M not found. 'example();' returns the list of known examples.~%") example)
           (return '$done)))))))

(defun mread-noprompt (&rest read-args)
  (let ((*mread-prompt* "") (*prompt-on-read-hang*))
    (declare (special *mread-prompt* *prompt-on-read-hang*))
    (unless read-args (setq read-args (list *query-io*)))
    (caddr (apply #'mread read-args))))

;; Some list creation utilities.

(defmspec $create_list (l)
  (cons '(mlist) (apply #'create-list1 (cadr l) (cddr l))))

(defun create-list1 (form &rest l &aux lis var1 top)
  (cond ((null l) (list (meval* form)))
	(t
	 (setq var1 (first l)
	       lis (second l)
	       l (cddr l))
	 (unless (symbolp var1) (merror (intl:gettext "create_list: expected a symbol; found: ~A") var1))
 	 (setq lis (meval* lis))
	 (progv (list var1)
	     (list nil)
	   (cond ((and (numberp lis)
		       (progn
			 (setq top (car l) l (cdr l))
			 (setq top (meval* top))
			 (numberp top)))
		  (loop for i from lis to top
		     do (setf (symbol-value var1) i)
		     append
		     (apply #'create-list1 form l)))
		 (($listp lis)
		  (loop for v in (cdr lis)
		     do (setf (symbol-value var1) v)
		     append
		     (apply #'create-list1 form l)))
		 (t (merror (intl:gettext "create_list: unexpected arguments."))))))))

;; The documentation is now in INFO format and can be printed using
;; tex, or viewed using info or gnu emacs or using a web browser.  All
;; versions of maxima have a builtin info retrieval mechanism.

(defmspec $describe (x)
  (let ((topic ($sconcat (cadr x)))
	(exact-p (or (null (caddr x)) (eq (caddr x) '$exact)))
	(cl-info:*prompt-prefix* *prompt-prefix*)
	(cl-info:*prompt-suffix* *prompt-suffix*))
    (if exact-p
	(cl-info:info-exact topic)
	(cl-info:info topic))))

; The old implementation
;(defun $apropos (s)
;  (cons '(mlist) (apropos-list s :maxima)))

;;; Utility function for apropos to filter a list LST with a function FN
;;; it is semiliar to remove-if-not, but take the return value of the function
;;; and build up a new list with this values.
;;; e.g. (filter #'(lambda(x) (if (oddp x) (inc x)) '(1 2 3 4 5)) --> (2 4 6)

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defmspec $apropos (s)
  (let (acc y)
    (setq s (car (margs s)))
    (cond ((stringp s)
           ;; A list of all Maxima names which contain the string S.
           (setq acc (append acc (apropos-list (stripdollar s) :maxima)))
           ;; Filter the names which are Maxima User symbols starting
           ;; with % or $ and remove duplicates.
           ($listify
             ($setify
               (cons '(mlist)
                      (filter #'(lambda (x)
                                  (cond ((eq (getcharn x 1) #\$) x)
                                        ((eq (getcharn x 1) #\%)
                                         ;; Change to a verb, when present.
                                         (if (setq y (get x 'noun))
                                             y
                                             x))
                                        (t nil)))
                              acc)))))
          (t
           (merror
             (intl:gettext "apropos: argument must be a string; found: ~M") s)))))


;; setup-info-database is exported to maxima package from cl-info
(defmfun $setup_help_database (&optional maxima-info-list)
  (setf maxima-info-list (cond (($listp maxima-info-list)
				(cdr maxima-info-list))
			       (t
				maxima-info-list)))
  (cl-info:setup-help-database :maxima-info-list maxima-info-list)
  '$done)

(defmfun $print_help_database (&optional (file nil))
  (cl-info:print-info-hashes file)
  '$done)

(defun $etags (&rest args-list)
  ;; most of this function is validation code
  ;; probably we could add a macro to do this
  (macrolet ((keys+validate (&rest l)
	       (let ((r '(list)))
		 (loop :for mk :in l
		    :do
		    (multiple-value-bind (mkey key validate-fn msg-string no-map-over binder) (values-list mk)
		      (declare (ignorable mkey key validate-fn msg-string no-map-over binder))
		      (setf validate-fn (or validate-fn #'stringp)
			    msg-string (or msg-string "list of strings"))
		      (push
		       `(list
			 ',mkey
			 ',key
			 #'(lambda (v)
			     (ignore-errors
				 (funcall ,(if no-map-over validate-fn #'(lambda(x) (and x (not (some validate-fn x))))) v)))
			 #'(lambda(value)
			     (error (intl:gettext (format nil "ETAGS: key ~(~a~) should be ~a, passed ~a. Halt." ,(subseq (symbol-name mkey) 1) ,msg-string value))))
			 ,(or binder #'(lambda(mk k v) (declare (ignorable mk)) (list k v)))
			 )
		       r)))
		 (reverse r))))
    (flet ((t-or-f (x) (or (eq x nil) (eq x t)))
	   (a-bind (mk k v) (declare (ignorable mk k v)) (list k (if v :append :supersede)))
	   (regex-test (l) (loop :for (re n) :on l :by #'cddr :unless (and (stringp re) (fixnump n)) :do (return-from regex-test nil)) t))
      (let* ((opts-list (keys+validate
                         ($file_regex_list     :file-regex-list										 )
                         ($directory_list      :directory-list										 )
                         ($regex_list          :regex-list          #'regex-test	"list of regex and registers"	 t               )
                         ($tags                :tags                #'stringp		"a string"			 t		 )
                         ($recursion_depth     :recursion-depth     #'fixnump		"an integer"			 t		 )
                         ($verbose             :verbose             #'t-or-f		"true or false"			 t		 )
                         ($append              :append-or-supersede #'t-or-f		"true or false"			 t       #'a-bind)
			 )))
	(labels ((get-lisp-list (l)
		   (if ($listp l) (cdr l) l))
		 (make-arg (a)
		   (setf a (get-lisp-list a))
		   (let ((key (car a))
			 (v   (cadr a)))
		     (loop :for opt :in opts-list
			:if (eq key (car opt))
			:do
			(multiple-value-bind (mk k validate-fn error-fn binder) (values-list opt)
			  (declare (ignorable mk k binder))
			  (unless (funcall validate-fn v) (funcall error-fn v))
			  (setf v (get-lisp-list v))
			  (return-from make-arg (funcall binder mk k v))))
		     (warn (intl:gettext (format nil "ETAGS did not recognize the option ~a. Skipping." key)))
		     nil))
		 (make-args-list ()
		   (loop :for arg :in args-list
		      :for k-v = (make-arg arg)
		      :when k-v :collect (car k-v)
		      :when k-v :collect (cadr k-v))))
	  (format t "~s~%" (make-args-list))
	  (apply #'etags:etags-create-tags-recursive (make-args-list))))
	  ;;(format t "~{~,,15,s=>~40T~s~^~%~}~%" (make-args-list))))
      )))