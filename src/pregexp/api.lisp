;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additions: Leo Butler 2011                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl-ppcre translations

#+gcl (defmacro with-standard-io-syntax (&rest rest) `(progn ,@rest))

(defmacro let-gs (l &body body)
  "Usage: (let-gs (a b c) body-using-a-b-and/or-c-as-gensym)"
  `(let ,(mapcar #'(lambda(x) `(,x (gensym (concatenate 'string (symbol-name ',x) "-")))) l) ,@body))

#|
(defmacro flatten (l)
  (let ((symbols (gensym "symbols")))
    `(let (,symbols)
       (labels ((flatten-l (l)
		  (cond ((consp l)
			 (flatten-l (car l))
			 (flatten-l (cdr l)))
			((null l)
			 (values))
			((symbolp l)
			 (push l ,symbols))
			(t
			 (values)))))
	 (flatten-l ',l)
	 (reverse (remove-duplicates ,symbols))))))

(defun safe-car (l)
  (cond ((consp l) (car l))
	((atom  l) l)
	(t nil)))
(defun safe-cadr (l)
  (cond ((consp l) (cadr l))
	(t nil)))

(defmacro let-gs (l &body body)
  "Usage: (let-gs (a b c) body-using-a-b-and/or-c-as-gensym)"
  (let ((gensyms+bindings (gensym "GB-")))
    `(let (,gensyms+bindings)
       (labels ((make-gensym (x)
		  (gensym (concatenate 'string (symbol-name x) "-")))
		(gensyms+bindings (l)
		  (let (x xg b)
		    (dolist (v l)
		      (setf x (safe-car v) xg (make-gensym x) b (safe-cadr v))
		      (push (list x `(quote ,xg)) ,gensyms+bindings)
		      (push (list xg b) ,gensyms+bindings))
		    (setf ,gensyms+bindings (reverse ,gensyms+bindings)))))
	 (gensyms+bindings ',l)
	 `(let* ,,gensyms+bindings
	    ,,@body)))))
|#

(declaim (inline compile-regex))
(defun compile-regex (re)
  (cond ((stringp re)
	 (pregexp re))
	((consp re)
	 re)
	((symbolp re)
	 `(compile-regex ,re))
	(t
	 (error "compile-regex re: re must be a string or s-exp."))))

(defmacro pregexp-flet-1 (name re &body body)
  "Takes a regex `re' (string) and produces a customised flet function
`name' which executes the regex function determined by `re' and
`body'."
  (let ((cre (compile nil `(lambda(string start end)
			     (pregexp-match-positions ,(compile-regex re) string start end)))))
    ;;    (format t "~a~%" cre)
    `(flet ((,name (string &optional (start 0) (end (length string)))
	      (funcall ,cre string start end)))
       ,@body)))

(defmacro pregexp-flet-1 (name re &body body)
  "Takes a regex `re' (string) and produces a customised flet function
`name' which executes the regex function determined by `re' and
`body'."
  (let ((cre (compile-regex re)))
    (format t "~a~%" cre)
    `(flet ((,name (string &optional (start 0) (end (length string)))
	      (pregexp-match-positions ,cre string start end)))
       ,@body)))

(defmacro pregexp-flet-1 (name re &body body)
  "Takes a regex `re' (string) and produces a customised flet function
`name' which executes the regex function determined by `re' and
`body'."
  (let-gs (cre)
    `(let ((,cre (compile-regex ,re)))
       ;;(format t "~a~%" ,cre)
       (flet ((,name (string &optional (start 0) (end (length string)))
		(pregexp-match-positions ,cre string start end)))
	 ,@body))))

					;(defmacro all-matches-as-strings (regex string &key (start 0) (end (length string)))
					;)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pregexp scan and scan-to-strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun pregexp-scan (re str &key (start 0) (end (length str)))
  (let* ((matches (pregexp-match-positions re str start end))
	 (b (match-begin matches))
	 (e (match-end matches))
	 (l (if matches (- (length matches) 1) 0))
	 (mb (make-array l :element-type 'fixnum))
	 (me (make-array l :element-type 'fixnum)))
    (do ((i 0             (1+ i))
	 (m (cdr matches) (cdr m)))
	((null m)
	 (values b e mb me))
      (setf (aref mb i) (match-begin m) (aref me i) (match-end m)))))

(defun pregexp-scan-to-strings (re str &key (start 0) (end (length str)))
  (let* ((matches (pregexp-match re str start end))
	 (b (car matches))
	 (l (if matches (- (length matches) 1) 0))
	 (ms (make-array l :element-type 'string :initial-element "")))
    (do ((i 0             (1+ i))
	 (m (cdr matches) (cdr m)))
	((null m)
	 (values b ms))
      (setf (aref ms i) (car m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline create-scanner parse-string))
(defun parse-string (string)
  (pregexp string))
(defun create-scanner (&rest args)
  (apply #'compile-regex args))
(defmacro match-begin (match)
  `(caar ,match))
(defmacro match-end (match)
  `(cdar ,match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl-ppcre api layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *scan*            (symbol-function 'pregexp-scan))
(defvar *scan-to-strings* (symbol-function 'pregexp-scan-to-strings))
(defvar *default-register-number* 10)

(defmacro do-scans ((match-start match-end reg-starts reg-ends regex string 
				 &optional result-form &key (start 0) (end (length string)))
		    &body body)
  ;; declaration* statement* => result*
  (let-gs (rgx str s e m-s m-e r-s r-e)
    `(let ((,rgx (compile-regex ,regex))
	   (,str ,string)
	   (,e   ,end)
	   ,match-start ,match-end ,reg-starts ,reg-ends)
       (do ((,s ,start (1+ (or ,match-start ,end)))
	    (,m-s ,match-start ,match-start)
	    (,m-e ,match-end   ,match-end  )
	    (,r-s ,reg-starts  ,reg-starts )
	    (,r-e ,reg-ends    ,reg-ends   ))
	   ((or (> ,s ,e)
		(null (multiple-value-setq (,match-start ,match-end ,reg-starts ,reg-ends) (funcall *scan* ,rgx ,str :start ,s :end ,e))))
	    (setf ,match-start ,m-s
		  ,match-end   ,m-e
		  ,reg-starts  ,r-s
		  ,reg-ends    ,r-e)
	    ,@result-form)
	 (progn
	   ,@body)))))

(do-scans (a b c d "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	     ((format t "~a~%" (list a b c d))))
  (values a b c d))

(let (e)
  (do-scans (a b c d "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	       (e))
    (push (list a b c d) e)))

'((22 24 #(22 23) #(23 24)) (8 10 #(8 9) #(9 10)))

(defmacro do-scans ((match-start match-end reg-starts reg-ends regex string 
				 &optional result-form &key (start 0) (end (length string)))
		    &body body)
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

(defun count-registers (regex)
  (if (stringp regex)
      (length (remove-if-not (lambda(x) (char= x #\()) (coerce regex 'list)))
      *default-register-number*))

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

(let (c)
  (do-scans-to-strings (mm rr "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			   ((reverse c)))
    (push (list mm rr) c)))

(do-scans-to-strings (mm rr "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			 ((values mm rr)))
  (format t "~a~%" (list mm rr)))


(defmacro do-matches ((match-start match-end regex string
				   &optional result-form &key (start 0) (end (length string)))
		      &body body)
  (let-gs (r-s r-e)
    `(do-scans (,match-start ,match-end ,r-s ,r-e ,regex ,string ,result-form :start ,start :end ,end)
       ,@body)))

(do-matches (a b "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	       ((values a b)))
  (format t "~a~%" (list a b)))


(defmacro do-matches-as-strings ((match regex string
					&optional result-form &key (start 0) (end (length string)))
				 &body body)
  (let-gs (register)
    `(do-scans-to-strings (,match ,register ,regex ,string ,result-form :start ,start :end ,end)
       ,@body)))


(do-matches-as-strings (a "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			  ((values a)))
  (format t "~a~%" (list a)))

(let (c)
  (do-matches-as-strings (a "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			    ((reverse c)))
    (push a c)))



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


(register-groups-bind (first second third fourth)
    ("((a)|(b)|(c))+" "abababc" :sharedp t)
  (list first second third fourth))
'("c" "a" "b" "c")



;; Local Variables:
;; eval: (fset 'slime-cp-block-to-repl "\C-[h\C-xrss\C-xo\C-xb*slime-repl\C-i\C-m\C-[>\C-u\C-xris\C-m\C-xo")
;; eval: (define-key lisp-mode-map "\C-cp" 'slime-cp-block-to-repl)
;; End: