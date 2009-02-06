;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module rpart)

;;;	Complex variable utilities
;;;
;;; Macsyma functions: $realpart $imagpart $rectform $polarform
;;;		       $cabs $carg
;;; Utility functions: trisplit risplit absarg cabs andmapc andmapcar

(load-macsyma-macros rzmac)

(declare-top (special $%emode $radexpand rp-polylogp $domain $m1pbranch
		      $logarc rischp $keepfloat complexsign))

(defmvar implicit-real nil "If t RPART assumes radicals and logs
         of real quantities are real and doesn't ask sign questions")

(defmvar generate-atan2 t "Controls whether RPART will generate ATAN's
	                or ATAN2's, default is to make ATAN2's")

(defmfun $realpart (xx) (car (trisplit xx)))

(defmfun $imagpart (xx) (cdr (trisplit xx)))

;;;Rectform gives a result of the form a+b*%i.

(defmfun $rectform (xx)
  (let ((ris (trisplit xx)))
    (add (car ris) (mul (cdr ris) '$%i))))

;;;Polarform gives a result of the form a*%e^(%i*b).

(defmfun $polarform (xx)
  (cond ((and (not (atom xx)) (member (caar xx) '(mequal mlist $matrix) :test #'eq))
	 (cons (car xx) (mapcar #'$polarform (cdr xx))))
	(t
	 (let ((aas (absarg xx)) ($%emode nil))
	   (mul (car aas) (powers '$%e (mul '$%i (cdr aas))))))))

;;; Cabs gives the complex absolute value.  Nota bene: an expression may
;;; be syntactically real without being real (e.g. sqrt(x), x<0).  Thus
;;; Cabs must lead an independent existence from Abs.

(defmfun $cabs (xx) (cabs ($rectform xx)))

;;; Carg gives the complex argument.
(defmfun $carg (xx)
  (cond ((and (not (atom xx)) (member (caar xx) '(mequal mlist $matrix) :test #'eq))
	 (cons (car xx) (mapcar #'$carg (cdr xx))))
	(t (cdr (absarg xx)))))

(defvar absflag nil)

;; The function of Absflag is to communicate to Absarg that only the absolute
;; value part of the result is wanted.  This allows Absarg to avoid asking
;; questions irrelevant to the absolute value.  For instance, Cabs(x) is
;; invariably Abs(x), while the complex phase may be 0 or %pi.  Note also
;; the steps taken in Absarg to assure that Asksign's will happen before Sign's
;; as often as possible, so that, for instance, Abs(x) can be simplified to
;; x or -x if the sign of x must be known for some other reason.  These
;; techniques, however, are not perfect.

;; The internal cabs, used by other Macsyma programs.
(defmfun cabs (xx) (let ((absflag t)) (car (absarg xx))))

;; Some objects can only appear at the top level of a legal simplified 
;; expression: CRE forms and equations in particular.

(defmfun trisplit (el)			;Top level of risplit
  (cond ((atom el) (risplit el))
	((specrepp el) (trisplit (specdisrep el)))
	((eq (caar el) 'mequal) (dot-sp-ri (cdr el) '(mequal simp)))
	(t (risplit el)))) 

;;; Auxiliaries

;; These are Macsyma equivalents to (mapcar 'trisplit ...).  They must
;; differ from other maps for two reasons: the lists are Macsyma lists,
;; and therefore prefixed with list indicators; and the results must
;; be separated: ((a . b) (c . d)) becomes something like ([a,c].[b,d]).

(defun dsrl (el) (dot-sp-ri (cdr el) '(mlist simp)))

(defun dot-sp-ri (el ind)
  (dot--ri (mapcar #'trisplit el) ind))

;; Dot--ri does the ((a.b)(c.d))->([a,c].[b,d]) transformation with
;; minimal Cons'ing.

(defun dot--ri (el ind) 
  (do ((i el (cdr i)) (k))
      ((null i) (cons (cons ind (nreverse k)) (cons ind el)))
    (let ((cdari (cdar i)))
      (setq k (rplacd (car i) k))
      (rplaca i cdari))))

(defun risplit-mplus (l)
  (do ((rpart) (ipart) (m (cdr l) (cdr m)))
      ((null m) (cons (addn rpart t) (addn ipart t)))
    (let ((sp (risplit (car m)))) 
      (cond ((=0 (car sp)))
	    (t (setq rpart (cons (car sp) rpart))))
      (cond ((=0 (cdr sp)))
	    (t (setq ipart (cons (cdr sp) ipart)))))))

(defun risplit-times (l)
  ((lambda (risl)
     (cond ((null (cdr risl)) (cons (muln (car risl) t) 0))
	   (t (do ((rpart 1) (ipart 0) (m (cdr risl) (cdr m))) 
		  ((null m)
		   (cons (muln (cons rpart (car risl)) t)
			 (muln (cons ipart (car risl)) t)))
		(psetq rpart (sub (mul rpart (caar m))
				  (mul ipart (cdar m)))
		       ipart (add (mul ipart (caar m))
				  (mul rpart (cdar m))))))))
   (do ((purerl nil) (compl nil) (l (cdr l) (cdr l)))
       ((null l) (cons purerl compl))
     ;;This is what Risl is bound to
     ((lambda (sp)
	(cond ((=0 (cdr sp)) (setq purerl (rplacd sp purerl)))
	      ((or (atom (car sp)) (atom (cdr sp)))
	       (setq compl (cons sp compl)))
	      ((and (eq (caaar sp) 'mtimes)
;;;Try risplit z/w and notice denominator.  If this check were not made,
;;; the real and imaginary parts would not each be over a common denominator.
		    (eq (caadr sp) 'mtimes)
		    (let ((nr (nreverse (cdar sp)))
			  (ni (nreverse (cddr sp))))
		      (cond ((equal (car nr) (car ni))
			     (setq purerl (cons (car nr) purerl)
				   compl (cons (cons (muln (nreverse (cdr nr)) t)
						     (muln (nreverse (cdr ni)) t))
					       compl)))
			    (t
			     (setq nr (nreverse nr))
			     (setq ni (nreverse ni))
			     nil)))))
	      (t (setq compl (cons sp compl)))))
      (risplit (car l))))))

(defun risplit-expt (l)
  ((lambda (pow $radexpand ris)	   ; Don't want 'simplifications' like
     (cond ((fixnump pow)		; Sqrt(-x) -> %i*sqrt(x)
	    ((lambda (sp)
	       (cond ((= pow -1)
		      ((lambda (a2+b2)
			 (cons (div (car sp) a2+b2)
			       (mul -1 (div (cdr sp) a2+b2))))
		       (spabs sp)))
		     ((> (abs pow) $maxposex)
		      (cond ((=0 (cdr sp)) (cons (powers (car sp) pow) 0))
			    (t ((lambda (abs^n natan)
				  (cons (mul abs^n
					     (take '(%cos) natan))
					(mul abs^n (take '(%sin) natan))))
				(powers (add (powers (car sp) 2)
					     (powers (cdr sp) 2))
					(*red pow 2))
				(mul pow (genatan (cdr sp) (car sp)))))))
		     ((> pow 0) (expanintexpt sp pow))
		     (t ((lambda (abbas basspli)
			   (cons (div (car basspli) abbas)
				 (neg (div (cdr basspli) abbas))))
			 (powers (spabs sp) (f- pow))
			 (expanintexpt sp (f- pow))))))
	     (risplit (cadr l))))
	   ((and (ratnump pow)
		 (fixnump (cadr pow))
		 (not (< (cadr pow) (f- $maxnegex)))
		 (not (> (cadr pow) $maxposex))
		 (prog2 (setq ris (risplit (cadr l)))
		     (or (= (caddr pow) 2) (=0 (cdr ris)))))
	    (cond ((=0 (cdr ris))
		   (case (cond ((mnegp (car ris)) '$negative)
			       (implicit-real '$positive)
			       (t (asksign (car ris))))
		     ($negative (risplit (mul2 (power -1 pow) (power (neg (car ris)) pow))))
		     ($zero (cons (power 0 pow) 0))
		     (t (cons (power (car ris) pow) 0))))
		  (t ((lambda (abs2 n pos?)
			((lambda (abs)
			   (divcarcdr
			    (expanintexpt 
			     (cons (power (add abs (car ris)) (1//2))
				   (porm ((lambda (a b) (cond (a (not b)) (b t))) ;Xor
					  pos? (eq (asksign (cdr ris)) '$negative))
					 (power (sub abs (car ris)) (1//2))))
			     n)
			    (cond (pos? (power 2 (div n 2)))
				  (t (power (mul 2 abs2) (div n 2))))))
			 (power abs2 (1//2))))
		      (spabs ris) (abs (cadr pow)) (> (cadr pow) -1)))))
	   ((and (floatp (setq ris (cadr l))) (floatp pow))
	    (risplit ((lambda ($numer) (exptrl ris pow)) t)))
	   (t ((lambda (sp aa)
		 ;;If all else fails, we use the trigonometric form.
		 ((lambda (pre post)
		    (cons (mul pre (take '(%cos) post))
			  (mul pre (take '(%sin) post))))
		  (mul (powers '$%e (mul (cdr aa) (mul (cdr sp) -1)))
		       (powers (car aa) (car sp)))
		  (add (mul (cdr sp) (take '(%log) (car aa)))
		       (mul (car sp) (cdr aa)))))
	       (risplit (caddr l)) (absarg1 (cadr l))))))
   (caddr l) nil nil))

(defun risplit-noun (l)
  (cons (simplify (list '(%realpart) l)) (simplify (list '(%imagpart) l))))

(defun absarg1 (arg)
  (let ((arg1 arg) ($keepfloat t))
    (cond ((and (or (free arg '$%i)
		    (free (setq arg1 (sratsimp arg)) '$%i))
		(not (eq (csign arg1) t)))
	   (setq arg arg1)
	   (if implicit-real
	       (cons arg 0)
	       (unwind-protect
		    (prog2 (assume `(($notequal) ,arg 0)) 
			(absarg arg))
		 (forget `(($notequal) ,arg 0)))))
	  (t (absarg arg)))))

;;;	Main function
;;; Takes an expression and returns the dotted pair
;;; (<Real part> . <imaginary part>).

(defun risplit (l) 
  (let (($domain '$complex) ($m1pbranch t) $logarc op)
    (cond ((atom l)
	   (cond ((eq l '$%i) (cons 0 1))
		 ((eq l '$infinity) (cons '$und '$und))
		 ((decl-complexp l) (risplit-noun l))
		 (t (cons l 0))))
	  ((eq (caar l) 'rat) (cons l 0))
	  ((eq (caar l) 'mplus) (risplit-mplus l))
	  ((eq (caar l) 'mtimes) (risplit-times l))
	  ((eq (caar l) 'mexpt) (risplit-expt l))
	  ((eq (caar l) '%log)
	   (let ((aa (absarg1 (cadr l))))
	     (rplaca aa (take '(%log) (car aa)))))
	  ((eq (caar l) 'bigfloat) (cons l 0)) ;All numbers are real.
	  ((and (member (caar l) '(%integrate %derivative %laplace %sum) :test #'eq)
		(freel (cddr l) '$%i))
	   (let ((ris (risplit (cadr l))))
	     (cons (simplify (list* (ncons (caar l)) (car ris) (cddr l)))
		   (simplify (list* (ncons (caar l)) (cdr ris) (cddr l))))))
	  (((lambda (ass)
;;;This clause handles the very similar trigonometric and hyperbolic functions.
;;; It is driven by the table at the end of the lambda.
	      (and ass
		   ((lambda (ri)
		      (cond ((=0 (cdr ri)) ;Pure real case.
			     (cons (take (list (car ass)) (car ri)) 0))
			    (t (cons (mul (take (list (car ass)) (car ri))
					  (take (list (cadr ass)) (cdr ri)))
				     (negate-if (eq (caar l) '%cos)
						(mul (take (list (caddr ass))
							   (car ri))
						     (take (list (cdddr ass))
							   (cdr ri))))))))
		    (risplit (cadr l)))))
	    (assoc (caar l)
		   '((%sin %cosh %cos . %sinh)
		     (%cos %cosh %sin . %sinh)
		     (%sinh %cos %cosh . %sin)
		     (%cosh %cos %sinh . %sin)) :test #'eq)))
	  ((member (caar l) '(%tan %tanh) :test #'eq)
	   ((lambda (sp)
;;;The similar tan and tanh cases.
	      (cond ((=0 (cdr sp)) (cons l 0))
		    (t 
		     ((lambda (2rl 2im)
			((lambda (denom)
			   (cond ((eq (caar l) '%tan)
				  (cons (mul (take '(%sin) 2rl) denom)
					(mul (take '(%sinh) 2im) denom)))
				 (t (cons (mul (take '(%sinh) 2rl) denom)
					  (mul (take '(%sin) 2im) denom)))))
			 (inv (cond ((eq (caar l) '%tan)
				     (add (take '(%cosh) 2im) (take '(%cos) 2rl)))
				    (t (add (take '(%cos) 2im) (take '(%cosh) 2rl)))))))
		      (mul (car sp) 2)
		      (mul (cdr sp) 2)) )))
	    (risplit (cadr l))))
	  ((and (member (caar l) '(%atan %csc %sec %cot %csch %sech %coth) :test #'eq)
		(=0 (cdr (risplit (cadr l)))))
	   (cons l 0))
	  ((and (eq (caar l) '$atan2) (=0 (cdr (risplit (div (cadr l) (caddr l))))))
	   (cons l 0))
	  ((or (arcp (caar l)) (eq (caar l) '$atan2))
	   (let ((ans (risplit ((lambda ($logarc) (ssimplifya l)) t))))
	     (cond ((eq (caar l) '$atan2)
		    (setq ans (cons (sratsimp (car ans)) (sratsimp (cdr ans))))))
	     (cond ((and (free l '$%i) (=0 (cdr ans))) (cons l 0)) (t ans))))
	  ((eq (caar l) '%plog)
	   ;;  (princ '|Warning: Principal value not guaranteed for Plog in Rectform/|)
	   (risplit (cons '(%log) (cdr l))))
	  ((member (caar l) '(%realpart %imagpart mabs) :test #'eq) (cons l 0))
	  ((eq (caar l) '%erf)
	   (let ((ris (risplit (cadr l))) orig cc)
	     (setq orig (simplify (list '(%erf) (add (car ris) (mul '$%i (cdr ris))))))
	     (setq cc (simplify (list '(%erf) (sub (car ris) (mul '$%i (cdr ris))))))
	     (cons (div (add orig cc) 2) (div (sub orig cc) (mul 2 '$%i)))))
          ;; Look for a risplit-function on the property list to handle the
          ;; realpart and imagpart for this function.
          ((setq func (get (mop l) 'risplit-function))
           (funcall func l))
;;; ^ All the above are guaranteed pure real.
;;; The handling of lists and matrices below has to be thought through.
	  ((eq (caar l) 'mlist) (dsrl l))
	  ((eq (caar l) '$matrix)
	   (dot--ri (mapcar #'dsrl (cdr l)) '($matrix simp)))
	  ((member (caar l) '(mlessp mleqp mgreaterp mgeqp) :test #'eq)
	   (let ((ris1 (risplit (cadr l))) (ris2 (risplit (caddr l))))
	     (cons (simplify (list (ncons (caar l)) (car ris1) (car ris2)))
		   (simplify (list (ncons (caar l)) (cdr ris1) (cdr ris2))))))
;;;The Coversinemyfoot clause covers functions which can be converted
;;; to functions known by risplit, such as the more useless trigonometrics.
	  (((lambda (foot) (and foot (risplit foot)))
	    (coversinemyfoot l)))
          ((or (get (mop l) 'commutes-with-conjugate)
               (get (mop l) 'conjugate-function))
           ;; A function with Mirror symmetry. The general expressions for 
           ;; the realpart and imagpart simplifies accordingly.
           (cons (mul (div 1 2)
                      (add (simplify (list '($conjugate) l)) l))
                 (mul (div 1 2) '$%i
                      (sub (simplify (list '($conjugate) l)) l))))
;;; A MAJOR ASSUMPTION:
;;;  All random functions are pure real, regardless of argument.
;;;  This is evidently assumed by some of the integration functions.
;;;  Perhaps the best compromise is to return 'realpart/'imagpart
;;;   under the control of a switch set by the integrators.  First
;;;   all such dependencies must be found in the integ
	  ((and rp-polylogp (mqapplyp l) (eq (subfunname l) '$li)) (cons l 0))
	  ((prog2 (setq op (if (eq (caar l) 'mqapply) (caaadr l) (caar l)))
	       (decl-complexp op))
	   (risplit-noun l))
	  ((and (eq (caar l) '%product) (not (free (cadr l) '$%i)))
	   (risplit-noun l))
	  (t (cons l 0)))))

(defun coversinemyfoot (l)
  (prog (recip)
     (cond ((not (member (caar l) '(%csc %sec %cot %csch %sech %coth) :test #'eq)))
	   ((null (setq recip (get (caar l) 'recip))))
	   (t (return (div 1 (cons (list recip) (cdr l))))))))

(defun powers (c d) 
  (cond ((=1 d) c)
	((equal d 0) 1)		      ;equal to preclude 0^(pdl 0)->0:
	((=0 c) 0)			; see comment before =0.
	((=1 c) 1)
	(t (power c d))))

(defun spabs (sp) (add (powers (car sp) 2) (powers (cdr sp) 2)))


(defvar negp* (let ((l (list nil nil t t))) (nconc l l)))

(defun divcarcdr (a b)
  (cons (div (car a) b) (div (cdr a) b)))


;;Expand bas^n, where bas is (<real part> . <imaginary part>)

(defun expanintexpt (bas n)
  (cond ((= n 1) bas)
        (t (do ((rp (car bas))
		(ip (cdr bas))
		(c 1 (quotient (* c ex) i))
		(ex n (1- ex)) (i 1 (1+ i))
		(rori t (not rori)) (negp negp* (cdr negp))
		(rpt nil) (ipt nil))
	       ((< ex 0) (cons (addn rpt t) (addn ipt t)))
	     (declare (fixnum ex i))
	     (set-either rpt ipt
			 rori
			 (cons (negate-if (car negp)
					  (mul c
					       (powers rp ex)
					       (powers ip (1- i))))
			       (cond (rori rpt) (t ipt))))))))

 

;;;   Subtract out multiples of 2*%pi with a minimum of consing.
;;;   Attempts to reduce to interval (-pi,pi].

(defun 2pistrip (exp)
  (cond ((atom exp) exp)
	((eq (caar exp) 'mtimes)
	 (cond ((and (mnump (cadr exp))
		     (eq (caddr exp) '$%pi)
		     (null (cdddr exp)))
		(cond ((integerp (cadr exp)) ;5*%pi
		       (mul (abs (rem (cadr exp) 2)) '$%pi))
					;Neither 0 nor 1 appears as a coef
		      ((and (listp (cadr exp))
			    (eq 'rat (caaadr exp))) ;5/2*%pi
		       (mul (list* '(rat simp)
				   (1- (rem (1+ (cadadr exp))
					    (* 2 (car (cddadr exp)))))
				   (cddadr exp))
			    '$%pi))
		      (t exp)))
	       (t exp)))
	((eq (caar exp) 'mplus)
	 ((lambda (res)
	    (cond ((eq res (cdr exp)) exp) (t (addn res t))))
	  (2pirec (cdr exp))))
	(t exp)))

(defun 2pirec (fm)			;Takes a list of exprs
  (cond ((null (cdr fm))		;If monad, just return.
	 ((lambda (2pf)
	    (cond ((eq 2pf (car fm)) fm)
		  ((=0 2pf) nil)
		  (t (list 2pf))))
	  (2pistrip (car fm))))
	(t ((lambda (2pfma 2pfmd)
	      (cond ((or (null 2pfmd) (=0 2pfmd)) 2pfma)
		    ((and (eq 2pfmd (cdr fm)) (eq 2pfma (car fm))) fm)
		    (t (cons 2pfma 2pfmd))))
	    (2pistrip (car fm)) (2pirec (cdr fm))))))

;;;	Rectify into polar form; Arguments similar to risplit

(defun argnum (n)
  (cond ((minusp n) (simplify '$%pi)) (t 0)))

(defun absarg (l)
  (setq l ($expand l))
  (cond ((atom l)
	 (cond ((eq l '$%i)
		(cons 1 (simplify '((mtimes) ((rat simp) 1 2) $%pi))))
	       ((numberp l)
		(cons (abs l) (argnum l)))
	       ((member l '($%e $%pi) :test #'eq) (cons l 0))
	       ((eq l '$infinity) (cons '$inf '$ind))
	       ((kindp l '$complex) (cons (list '($cabs) l)
					  (list '($carg) l)))
	       (absflag (cons (take '(mabs) l) 0))
	       (t ((lambda (gs)
		     (cond ((eq gs '$positive) (cons l 0))
			   ((eq gs '$zero) (cons 0 0))
			   ((eq gs '$negative)
			    (cons (neg l) (simplify '$%pi)))
			   (t (cons (take '(mabs) l) 0))))
		   (cond ((eq rischp l) '$positive) (t (asksign l)))))))
	((member (caar l) '(rat bigfloat) :test #'eq)
	 (cons (list (car l) (abs (cadr l)) (caddr l))
	       (argnum (cadr l))))
	((eq (caar l) 'mtimes)
	 (do ((n (cdr l) (cdr n))
	      (abars)
	      (argl () (cons (cdr abars) argl))
	      (absl () (rplacd abars absl)))
	     (())
	   (cond ((not n)
		  (return (cons (muln absl t)
				(2pistrip (addn argl t))))))
	   (setq abars (absarg (car n)))))
	((eq (caar l) 'mexpt)
	 (let ((aa (absarg (cadr l)))
	       (sp (risplit (caddr l)))
	       ($radexpand nil))
	   (cons (mul (powers (car aa) (car sp))
		      (powers '$%e (neg (mul (cdr aa) (cdr sp)))))
		 (add (mul (cdr aa) (car sp))
		      (mul (cdr sp) (take '(%log) (car aa)))))))
	((and (member (caar l) '(%tan %tanh) :test #'eq)
	      (not (=0 (cdr (risplit (cadr l))))))
	 ((lambda (sp)
	    ((lambda (2frst 2scnd)
	       (cond ((eq (caar l) '%tanh)
		      (psetq 2frst 2scnd 2scnd 2frst)))
	       (cons ((lambda (cosh cos)
			(root (div (add cosh (neg cos))
				   (add cosh cos))
			      2))
		      (take '(%cosh) 2frst)
		      (take '(%cos) 2scnd))
		     (take '(%atan)
			   (cond ((eq (caar l) '%tan)
				  (div (take '(%sinh) 2frst)
				       (take '(%sin) 2scnd)))
				 (t (div (take '(%sin) 2frst)
					 (take '(%sinh) 2scnd)))))))
	     (mul (cdr sp) 2)
	     (mul (car sp) 2)))
	  (risplit (cadr l))))
	((specrepp l) (absarg (specdisrep l)))
	(((lambda (foot)
	    (and foot (not (=0 (cdr (risplit (cadr l))))) (absarg foot)))
	  (coversinemyfoot l)))
	(t (let ((ris (trisplit l)))
	     (xcons
;;; Arguments must be in this order so that the side-effect of the Atan2,
;;; that is, determining the Asksign of the argument, can happen before
;;; Take Mabs does its Sign.  Blame JPG for noticing this lossage.
	      (if absflag 0 (genatan (cdr ris) (car ris)))
	      (cond ((equal (car ris) 0) (absarg-mabs (cdr ris)))
		    ((equal (cdr ris) 0) (absarg-mabs (car ris)))
		    (t (powers ($expand (add (powers (car ris) 2)
					     (powers (cdr ris) 2))
					1 0)
			       (half)))))))))

(defun genatan (num den)
  (let ((arg (take '($atan2) num den)))
    (if (or generate-atan2 (free arg '$atan2))
	arg
	(take '(%atan) (m// num den)))))

(defun absarg-mabs (l)
  (if (eq (csign l) t)
      (if (member (caar l) '(mabs %cabs) :test #'eq) l (list '(%cabs simp) l))
      (take '(mabs) l)))
