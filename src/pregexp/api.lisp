;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl-ppcre translations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+pregexp
(in-package :pregexp)
#+maxima-nregex
(in-package :maxima-nregex)

(defmacro let-gs (l &body body)
  "Usage: (let-gs (a b c) body-using-a-b-and/or-c-as-gensym)"
  `(let ,(mapcar #'(lambda(x) `(,x (gensym (concatenate 'string (symbol-name ',x) "-")))) l) ,@body))

(defmacro register-groups-bind (var-list
				(regex string &key (start 0) (end (length string)) sharedp)
				&body body)
  (declare (ignorable sharedp))
  (let-gs (match registers)
	  (let* ((l (count-registers regex))
		 (bindings (loop for v in `(,@var-list)
			      for i = 0 then (1+ i)
			      collect `(setf ,v (if (< ,i ,l) (aref ,registers ,i))))))
	    `(let* (,@var-list ,match ,registers)
	       (multiple-value-setq (,match ,registers) (funcall *scan-to-strings* ,regex ,string :start ,start :end ,end))
	       (if ,match
		   (progn
		     ,@bindings
		     ,@body))))))

(defmacro do-scans ((match-start match-end reg-starts reg-ends regex string 
				 &optional result-form &key (start 0) (end (length string)) (sharedp t))
		    &body body)
  (declare (ignorable sharedp))
  (let-gs (rgx str s e m-s m-e r-s r-e fn)
    `(let ((,rgx (compile-regex ,regex))
	   (,str ,string)
	   (,e   ,end)
	   ,match-start ,match-end ,reg-starts ,reg-ends
	   ,m-s ,m-e ,r-s ,r-e)
       (labels ((,fn (,s)
		  (multiple-value-setq (,match-start ,match-end ,reg-starts ,reg-ends) (funcall *scan* ,rgx ,str :start ,s :end ,e))
		  (and ,match-start
		       (setf ,m-s ,match-start
			     ,m-e ,match-end  
			     ,r-s ,reg-starts 
			     ,r-e ,reg-ends)
		       (progn ,@body t)
		       (funcall #',fn (1+ ,match-start)))))
	 (funcall #',fn ,start)
	 (setf ,match-start ,m-s
	       ,match-end   ,m-e
	       ,reg-starts  ,r-s
	       ,reg-ends    ,r-e)
	 ,@result-form))))

(defmacro do-scans-to-strings ((match register regex string 
				      &optional result-form &key (start 0) (end (length string)))
			       &body body)
  (let-gs (m-s m-e r-s r-e)
    (let* ((l-r (1- (count-registers regex)))
	   (set-match-string+register
	    `((setf ,match (if ,m-s (subseq ,string ,m-s ,m-e)))
	      (loop for i from 0 to ,l-r
		 for regi = (if (< i (length ,r-s))
				(subseq ,string (aref ,r-s i) (aref ,r-e i)))
		 do (setf (aref ,register i) regi)))))
      (setf body         (append set-match-string+register body)
	    result-form  (append set-match-string+register result-form))
      `(let (,match
	     (,register (make-array (1+ ,l-r) :element-type 'string :initial-element "")))
	 (do-scans (,m-s ,m-e ,r-s ,r-e ,regex ,string ,result-form :start ,start :end ,end)
	   ,@body)))))

(defmacro do-matches ((match-start match-end regex string
				   &optional result-form &key (start 0) (end (length string)))
		      &body body)
  (let-gs (r-s r-e)
    `(do-scans (,match-start ,match-end ,r-s ,r-e ,regex ,string ,result-form :start ,start :end ,end)
       ,@body)))

(defmacro do-matches-as-strings ((match regex string
					&optional result-form &key (start 0) (end (length string)))
				 &body body)
  (let-gs (register)
    `(do-scans-to-strings (,match ,register ,regex ,string ,result-form :start ,start :end ,end)
       ,@body)))

(defmacro do-register-groups  (var-list
			       (regex string &optional result-form &key (start 0) (end (length string)) sharedp)
			       &body body)
  (declare (ignorable sharedp))
  (let-gs (match-start match-end register-start register-end s)
	  (let* ((l (count-registers regex))
		 (bindings (loop for v in `(,@var-list)
			      for i = 0 then (1+ i)
			      collect `(setf ,v (if (and (< ,i ,l) (< ,i (length ,register-start)) (aref ,register-start ,i))
						    (subseq ,s (aref ,register-start ,i) (aref ,register-end ,i))))))
		 )
	    `(let* (,@var-list
		    (,s ,string))
	       (do-scans (,match-start ,match-end ,register-start ,register-end ,regex ,s ,result-form :start ,start :end ,end :sharedp ,sharedp)
		 ,@bindings
		 ,@body)))))

(defun all-matches (regex string &key (start 0) (end (length string)))
  (let ((matches '()))
    (do-matches (s e regex string
		   ((reverse matches))
		   :start start :end end)
      (push s matches)
      (push e matches))))

(defun all-matches-as-strings (regex string &key (start 0) (end (length string)))
  (let ((matches '()))
    (do-matches-as-strings (m regex string
			      ((reverse matches))
			      :start start :end end)
      (push m matches))))


;; end of api.lisp 
