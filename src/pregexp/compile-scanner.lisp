;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

(defmacro compile-scanner (scanner &optional (list-or-lambda t))
  (setf scanner (cond ((stringp scanner)
		       (pregexp scanner))
		      (t
		       scanner)))
  (format t "Scanner: ~S~%" scanner)
  (labels ((or-and (scanner)
	     (let ((rest (cdr scanner))
		   (op (intern (symbol-name (car scanner)))))
	       (cond ((consp rest)
		      (let ((r (loop for r in rest
				   for c = (c-s r)
				   collect c)))
			`(,op ,@r)))
		     ((atom rest)
		      (c-s rest)))))
	   (c-s (scanner)
	     (format t "C-S: ~S~%" scanner)
	     (cond ((atom scanner)
		    (return-from c-s scanner)))
	     (case (car scanner)
	       (:sub
		(let ((sub (c-s (cadr scanner))))
		  (if list-or-lambda
		      `(list ',sub)
		      `(lambda (string)
			 (declare (ignorable string))
			 ,sub))))
	       ((:or :and)
		(or-and scanner))
	       (otherwise
		`(progn ,scanner)))))
    (c-s scanner)))
    

  (let* ((backrefs (pregexp-make-backref-list re))
         (case-sensitive-p t)
	 (*pregexp-single-line-mode-p* nil)
	 (*pregexp-multi-line-mode-p* nil))
    (flet ((char=1 (c1 c2)
	     (if case-sensitive-p
		 (char= c1 c2)
		 (char-equal c1 c2)))
           (char<=1 (c1 c2 c3)
	     (if case-sensitive-p
		 (char<= c1 c2 c3)
		 (char-not-greaterp c1 c2 c3))))
      

;; end of compile.lisp 
