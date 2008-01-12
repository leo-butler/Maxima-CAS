;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;; When non-NIL, the Bessel functions of half-integral order are
;; expanded in terms of elementary functions.

(defmvar $besselexpand nil)

(defvar $iarray)
(defvar $yarray)
(defvar $besselarray)

;;
;; Bessel function of the first kind of order 0.
;;
;; One definition is
;;
;;         INF
;;         ====       k  2 k
;;         \     (- 1)  z
;;          >    -----------
;;         /       2 k   2
;;         ====   2    k!
;;         k = 0
;;
;; We only support computing this for real z.
;;

;; rtoy: Commenting these out for now because they're deprecated and
;; we have equivalent functionality in bessel_j and bessel_i.
;;
;; XXX  Remove these someday.

#||
(defun j[0]-bessel (x)
  (slatec:dbesj0 (float x 1d0)))

(defun $j0 ($x)
  "J[0](x). This is deprecated.  Use bessel_j(0,x)"        
  (cond ((numberp $x)
	 (j[0]-bessel (float $x)))
	(t (list '(%bessel_j simp) 0 $x))))


;; Bessel function of the first kind of order 1.
;;
;; One definition is
;;
;;      INF
;;      ====   - 2 k - 1      k  2 k + 1
;;      \     2          (- 1)  z
;;       >    --------------------------
;;      /            k! (k + 1)!
;;      ====
;;      k = 0

(defun j[1]-bessel (x) 
  (slatec:dbesj1 (float x 1d0)))

(defun $j1 ($x)
  "J[1](x).  This is deprecated.  Use bessel_j(1,x)"
  (cond ((numberp $x)
	 (j[1]-bessel (float $x)))
	(t (list '(%bessel_j simp) 1 $x))))

;; Bessel function of the first kind of order n
;;
;; The order n must be a non-negative real.
(defun $jn ($x $n)
  "J[n](x).  This is deprecated.  Use bessel_j(n,x)"
  (cond ((and (numberp $x) (numberp $n) (>= $n 0))
	 (multiple-value-bind (n alpha)
	     (floor (float $n))
	   (let ((jvals (make-array (1+ n) :element-type 'double-float)))
	     (slatec:dbesj (float $x) alpha (1+ n) jvals 0)
	     (narray $jarray n)
	     (fillarray (symbol-value '$jarray) jvals)
	     (aref jvals n))))
	(t (list '(%bessel_j simp) $n $x))))


;; Modified Bessel function of the first kind of order 0.  This is
;; related to J[0] via
;;
;; I[0](z) = J[0](z*exp(%pi*%i/2))
;;
;; and
;;
;;        INF
;;        ====         2 k
;;        \           z
;;         >    ----------------
;;        /         2 k   2 
;;        ====     2    k!
;;        k = 0

(defun i[0]-bessel (x)
  (slatec:dbesi0 (float x 1d0)))

(defun $i0 ($x)
  "I[0](x).  This is deprecated.  Use bessel_i(0,x)"
  (cond ((numberp $x)
	 (i[0]-bessel (float $x)))
	(t (list '(%bessel_i simp) 0 $x))))

;; Modified Bessel function of the first kind of order 1.  This is
;; related to J[1] via
;;
;; I[1](z) = exp(-%pi*%I/2)*J[0](z*exp(%pi*%i/2))
;;
;; and
;;
;;       INF
;;       ====         2 k
;;       \           z
;;        >    ----------------
;;       /      2 k
;;       ====  2    k! (k + 1)!
;;       k = 0

(defun i[1]-bessel (x)
  (slatec:dbesi1 (float x 1d0)))

(defun $i1 ($x)
  "I[1](x).  This is deprecated.  Use bessel_i(1,x)"
  (cond ((numberp $x) (i[1]-bessel (float $x)))
	(t (list '(%bessel_i simp) 1 $x))))

;; Modified Bessel function of the first kind of order n, where n is a
;; non-negative real.

||#

(defun bessel-i (order arg)
  (cond ((zerop (imagpart arg))
	 ;; We have numeric args and the first arg is purely
	 ;; real. Call the real-valued Bessel function.  We call i0
	 ;; and i1 instead of jn, if possible.
	 (let ((arg (realpart arg)))
	   (cond ((zerop order)
		  (slatec:dbesi0 (float arg)))
		 ((= order 1)
		  (slatec:dbesi1 (float arg)))
		 (t
		  (multiple-value-bind (n alpha) (floor (float order))
		    (setf $besselarray (make-array (1+ n) :element-type 'double-float))
		    (slatec:dbesi (float (realpart arg)) alpha 1 (1+ n) $besselarray 0)
		    (aref $besselarray n))))))
	(t
	 ;; The arg is complex.  Use the complex-valued Bessel
	 ;; function
	 (multiple-value-bind (n alpha)
	     (floor (float order))
	   (let ((cyr (make-array (1+ n) :element-type 'double-float))
		 (cyi (make-array (1+ n) :element-type 'double-float)))
	     (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
					v-cyr v-cyi v-nz v-ierr)
		 (slatec::zbesi (float (realpart arg))
				(float (imagpart arg))
				alpha
				1
				(1+ n)
				cyr
				cyi
				0
				0)
	       (declare (ignore v-zr v-zi v-fnu v-kode v-n v-cyr v-cyi v-nz))

	       ;; We should check for errors here based on the
	       ;; value of v-ierr.
	       (when (plusp v-ierr)
		 (format t "zbesi ierr = ~A~%" v-ierr))
	       (setf $besselarray (make-array (+ n 2)))
	       (dotimes (k (1+ n) (aref $besselarray n))
		 (setf (aref $besselarray k)
		       (simplify (list '(mplus)
				       (simplify (list '(mtimes)
						       '$%i
						       (aref cyi k)))
				       (aref cyr k)))))))))))

(defun bessel-k (order arg)
  (cond ((zerop (imagpart arg))
	 ;; We have numeric args and the first arg is purely
	 ;; real. Call the real-valued Bessel function.  We call i0
	 ;; and i1 instead of jn, if possible.
	 (let ((arg (realpart arg)))
	   (cond ((zerop order)
		  (slatec:dbesk0 (float arg)))
		 ((= order 1)
		  (slatec:dbesk1 (float arg)))
		 (t
		  ;; From A&S 9.6.6, K(-v,z) = K(v,z), so take the
		  ;; absolute value of the order.
		  (multiple-value-bind (n alpha) (floor (abs (float order)))
		    (setf $besselarray (make-array (1+ n) :element-type 'double-float))
		    (slatec:dbesk (float (realpart arg)) alpha 1 (1+ n) $besselarray 0)
		    (aref $besselarray n))))))
	(t
	 ;; The first arg is complex.  Use the complex-valued Bessel
	 ;; function
	 (multiple-value-bind (n alpha)
	     (floor (float order))
	   (let ((cyr (make-array (1+ n) :element-type 'double-float))
		 (cyi (make-array (1+ n) :element-type 'double-float)))
	     (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
					v-cyr v-cyi v-nz v-ierr)
		 (slatec::zbesk (float (realpart arg))
				(float (imagpart arg))
				alpha
				1
				(1+ n)
				cyr
				cyi
				0
				0)
	       (declare (ignore v-zr v-zi v-fnu v-kode v-n
				v-cyr v-cyi v-nz))

	       ;; We should check for errors here based on the
	       ;; value of v-ierr.
	       (when (plusp v-ierr)
		 (format t "zbesk ierr = ~A~%" v-ierr))
	       (setf $besselarray (make-array (+ n 2)))
	       (dotimes (k (1+ n) (aref $besselarray n))
		 (setf (aref $besselarray k)
		       (simplify (list '(mplus)
				       (simplify (list '(mtimes)
						       '$%i
						       (aref cyi k)))
				       (aref cyr k)))))))))))


;; I think g0(x) = exp(-x)*I[0](x), g1(x) = exp(-x)*I[1](x), and
;; gn(x,n) = exp(-x)*I[n](x), based on some simple numerical
;; evaluations.

(defun $scaled_bessel_i0 ($x)
  (cond ((mnump $x)
	 ;; XXX Should we return noun forms if $x is rational?
	 (slatec:dbsi0e ($float $x)))
	(t
	 (mul (power '$%e (neg (simplifya `((mabs) ,$x) nil)))
	      `((%bessel_i) 0 ,$x)))))

(defun $scaled_bessel_i1 ($x)
  (cond ((mnump $x)
	 ;; XXX Should we return noun forms if $x is rational?
	 (slatec:dbsi1e ($float $x)))
	(t
	 (mul (power '$%e (neg (simplifya `((mabs) ,$x) nil)))
	      `((%bessel_i) 1 ,$x)))))


(defun $scaled_bessel_i ($n $x)
  (cond ((and (mnump $x) (mnump $n))
	 ;; XXX Should we return noun forms if $n and $x are rational?
	 (multiple-value-bind (n alpha) (floor ($float $n))
	   (setf $iarray (make-array (1+ n) :element-type 'double-float))
	   (slatec:dbesi ($float $x) alpha 2 (1+ n) $iarray 0)
	   (aref $iarray n)))
	(t
	 (mul (power '$%e (neg (simplifya `((mabs) ,$x) nil)))
	      ($bessel_i $n $x)))))


;; Numerically compute H1(v, z).
;;
;; A&S 9.1.3 says H1(v,z) = J(v,z) + i * Y(v,z)
;;
(defun hankel-1 (v z)
  (let ((v (float v))
	(z (coerce z '(complex double-float))))
    (cond ((minusp v)
	   ;; A&S 9.1.6:
	   ;;
	   ;; H1(-v,z) = exp(v*pi*i)*H1(v,z)
	   ;;
	   ;; or
	   ;;
	   ;; H1(v,z) = exp(-v*pi*i)*H1(-v,z)
	   
	   (* (cis (* pi (- v))) (hankel-1 (- v) z)))
	  (t
	   (multiple-value-bind (n fnu)
	       (floor v)
	   (let ((zr (realpart z))
		 (zi (imagpart z))
		 (cyr (make-array (1+ n) :element-type 'double-float))
		 (cyi (make-array (1+ n) :element-type 'double-float)))
	     (multiple-value-bind (dzr dzi df dk dm dn dcyr dcyi nz ierr)
		 (slatec::zbesh zr zi fnu 1 1 (1+ n) cyr cyi 0 0)
	       (declare (ignore dzr dzi df dk dm dn dcyr dcyi nz ierr))
	       (complex (aref cyr n)
			(aref cyi n)))))))))

;; Numerically compute H2(v, z).
;;
;; A&S 9.1.4 says H2(v,z) = J(v,z) - i * Y(v,z)
;;
(defun hankel-2 (v z)
  (let ((v (float v))
	(z (coerce z '(complex double-float))))
    (cond ((minusp v)
	   ;; A&S 9.1.6:
	   ;;
	   ;; H2(-v,z) = exp(-v*pi*i)*H1(v,z)
	   ;;
	   ;; or
	   ;;
	   ;; H2(v,z) = exp(v*pi*i)*H1(-v,z)
	   
	   (* (cis (* pi v)) (hankel-2 (- v) z)))
	  (t
	   (multiple-value-bind (n fnu)
	       (floor v)
	   (let ((zr (realpart z))
		 (zi (imagpart z))
		 (cyr (make-array (1+ n) :element-type 'double-float))
		 (cyi (make-array (1+ n) :element-type 'double-float)))
	     (multiple-value-bind (dzr dzi df dk dm dn dcyr dcyi nz ierr)
		 (slatec::zbesh zr zi fnu 1 2 (1+ n) cyr cyi 0 0)
	       (declare (ignore dzr dzi df dk dm dn dcyr dcyi nz ierr))
	       (complex (aref cyr n)
			(aref cyi n)))))))))

;; Bessel function of the first kind for real or complex arg and real
;; non-negative order.
(defun $bessel ($arg $order)
  "bessel(arg, order) = J[order](arg). This is deprecated.  Use bessel_j(order,arg)"
  (cond ((not (bessel-numerical-eval-p $order $arg))
	 ;; Args aren't numeric.  Return unevaluated.
	 (list '(%bessel_j simp) $order $arg))
	((zerop ($imagpart $arg))
	 ;; We have numeric args and the first arg is purely
	 ;; real. Call the real-valued Bessel function.  (Should we
	 ;; try calling j0 and j1 as appropriate instead of jn?)
	 (cond ((= $order 0)
		(slatec:dbesj0 (float $arg)))
	       ((= $order 1)
		(slatec:dbesj1 (float $arg)))
	       ((minusp $order)
		;; Bessel function of negative order.  We use the
		;; Hankel function to compute this, because A&S 9.1.3
		;; says H1(v,z) = J(v,z) + i * Y(v,z), and we know
		;; J(v,z) is real.
		(realpart (hankel-1 $order $arg)))
	       (t
		(multiple-value-bind (n alpha) (floor (float $order))
		  (let ((jvals (make-array (1+ n) :element-type 'double-float)))
		    ;; Use analytic continuation formula A&S 9.1.35:
		    ;;
		    ;; %j[v](z*exp(m*%pi*%i)) = exp(m*%pi*%i*v)*%j[v](z)
		    ;;
		    ;; for an integer m.  In particular, for m = 1:
		    ;;
		    ;; %j[v](-x) = exp(v*%pi*%i)*%j[v](x)
		    (cond ((>= $arg 0)
			   (slatec:dbesj (float $arg) alpha (1+ n) jvals 0)
			   (setf $besselarray (make-array (1+ n) :initial-contents jvals)))
			  (t
			   (slatec:dbesj (- (float $arg)) alpha (1+ n) jvals 0)
			   (setf $besselarray (make-array (1+ n)))
			   (let ((s (cis (* $order pi))))
			     (dotimes (k (1+ n))
			       (let ((v (* s (aref jvals k))))
				 (setf (aref $besselarray k)
				       (simplify `((mplus) ,(realpart v)
						   ((mtimes)
						    $%i
						    ,(imagpart v))))))))))
		    (aref $besselarray n))))))
	(t
	 ;; The first arg is complex.  Use the complex-valued Bessel
	 ;; function.
	 (cond ((mminusp $order)
		;; Bessel function of negative order.  We use the
		;; Hankel function to compute this, because A&S 9.1.3
		;; says H1(v,z) = J(v,z) + i * Y(v,z), and H2(v,z) =
		;; J(v,z) - i * Y(v,z).  Thus, J(v,z) = (H1(v,z) +
		;; H2(v,z))/2.  Not the most efficient way, but
		;; perhaps good enough for maxima.
		(let* ((arg (complex ($realpart $arg)
				     ($imagpart $arg)))
		       (result (* 0.5d0 (+ (hankel-1 $order arg)
					   (hankel-2 $order arg)))))
		  (complexify result)))
	       (t
		(multiple-value-bind (n alpha)
		    (floor (float $order))
		  (let ((cyr (make-array (1+ n) :element-type 'double-float))
			(cyi (make-array (1+ n) :element-type 'double-float)))
		    (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
					       v-cyr v-cyi v-nz v-ierr)
			(slatec:zbesj (float ($realpart $arg))
				      (float ($imagpart $arg))
				      alpha
				      1
				      (1+ n)
				      cyr
				      cyi
				      0
				      0)
		      (declare (ignore v-zr v-zi v-fnu v-kode v-n
				       v-cyr v-cyi v-nz))

		      ;; Should check the return status in v-ierr of this
		      ;; routine.
		      (when (plusp v-ierr)
			(format t "zbesj ierr = ~A~%" v-ierr))
		      (setf $besselarray (make-array (+ n 2)))
		      (dotimes (k (1+ n) (aref $besselarray n))
			(setf (aref $besselarray k)
			      (simplify (list '(mplus)
					      (simplify (list '(mtimes)
							      '$%i
							      (aref cyi k)))
					      (aref cyr k)))))))))))))

(defmfun $bessel_j (v z)
  (simplify (list '(%bessel_j) (resimplify v) (resimplify z))))

;; Bessel function of the second kind, Y[n](z), for real or complex z
;; and non-negative real n.
(defun bessel-y (order arg)
  (cond ((zerop (imagpart arg))
	 ;; We have numeric args and the first arg is purely
	 ;; real. Call the real-valued Bessel function.
	 ;;
	 ;; For negative values, use the analytic continuation formula
	 ;; A&S 9.1.36:
	 ;;
	 ;; %y[v](z*exp(m*%pi*%i)) = exp(-v*m*%pi*%i) * %y[v](z)
	 ;;       + 2*%i*sin(m*v*%pi)*cot(v*%pi)*%j[v](z)
	 ;;
	 ;; In particular for m = 1:
	 ;;
	 ;; %y[v](-z) = exp(-v*%pi*%i) * %y[v](z) + 2*%i*cos(v*%pi)*%j[v](z)
	 ;; 
	 (let ((arg (realpart arg)))
	   (cond ((zerop order)
		  (cond ((>= arg 0)
			 (slatec:dbesy0 (float arg)))
			(t
			 ;; For v = 0, this simplifies to
			 ;;
			 ;; %y[0](-z) = %y[0](z) + 2*%i*%j[0](z)
			 (simplify `((mplus)
				     ,(slatec:dbesy0 (float (- arg)))
				     ((mtimes)
				      $%i
				      ,(* 2 (slatec:dbesj0 (float (- arg))))))))))
		 ((= order 1)
		  (cond ((>= arg 0)
			 (slatec:dbesy1 (float arg)))
			(t
			 ;; For v = 1, this simplifies to
			 ;;
			 ;; %y[1](-z) = -%y[1](z) - 2*%i*%j[1](v)
			 (simplify `((mplus)
				     ,(slatec:dbesy1 (float (- arg)))
				     ((mtimes)
				      $%i
				      ,(* -2 (slatec:dbesj1 (float (- arg))))))))))
		 (t
		  (multiple-value-bind (n alpha) (floor (float order))
		    (let ((jvals (make-array (1+ n) :element-type 'double-float)))
		      (cond ((>= arg 0)
			     (slatec:dbesy (float (realpart arg)) alpha (1+ n) jvals)
			     (setf $besselarray (make-array (1+ n) :initial-contents jvals))
			     (aref jvals n))
			    (t
			     (let* ((s1 (cis (- (* order pi))))
				    (s2 (* #c(0 2) (cos (* order pi)))))
			       (slatec:dbesy (- (float arg)) alpha (1+ n) jvals)
			       (setf $yarray (make-array (1+ n)))
			       (dotimes (k (1+ n))
				 (let ((v (+ (* s1 (aref jvals k))
					     (* s2 (aref $besselarray k)))))
				   (setf (aref $yarray k)
					 (simplify `((mplus) ,(realpart v)
						     ((mtimes)
						      $%i
						      ,(imagpart v)))))))
			       (aref $yarray n))))))))))
	(t
	 ;; The first arg is complex.  Use the complex-valued Bessel
	 ;; function
	 (multiple-value-bind (n alpha)
	     (floor (float order))
	   (let ((cyr (make-array (1+ n) :element-type 'double-float))
		 (cyi (make-array (1+ n) :element-type 'double-float))
		 (cwrkr (make-array (1+ n) :element-type 'double-float))
		 (cwrki (make-array (1+ n) :element-type 'double-float)))
	     (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
					v-cyr v-cyi v-nz
					v-cwrkr v-cwrki v-ierr)
		 (slatec::zbesy (float (realpart arg))
				(float (imagpart arg))
				alpha
				1
				(1+ n)
				cyr
				cyi
				0
				cwrkr
				cwrki
				0)
	       (declare (ignore v-zr v-zi v-fnu v-kode v-n
				v-cyr v-cyi v-cwrkr v-cwrki v-nz))

	       ;; We should check for errors here based on the
	       ;; value of v-ierr.
	       (when (plusp v-ierr)
		 (format t "zbesy ierr = ~A~%" v-ierr))
	       (setf $besselarray (make-array (+ n 2)))
	       (dotimes (k (1+ n) (aref $besselarray n))
		 (setf (aref $besselarray k)
		       (simplify (list '(mplus)
				       (simplify (list '(mtimes)
						       '$%i
						       (aref cyi k)))
				       (aref cyr k)))))))))))


(defun z-function (x y) 
  ((lambda (xs ys capn nu np1 h h2 lamb r1 r2 s s1 s2 t1 t2 c bool re im) 
     (setq xs (cond ((> 0.0d0 x) -1.0d0) (t 1.0d0)))
     (setq ys (cond ((> 0.0d0 y) -1.0d0) (t 1.0d0)))
     (setq x (abs x) y (abs y))
     (cond ((and (> 4.29d0 y) (> 5.33d0 x))
	    (setq s (* (1+ (* -0.23310023d0 y))
			(sqrt (1+ (* -0.035198873d0 x x)))))
	    (setq h (* 1.6d0 s) h2 (* 2.0d0 h) capn (+ 6 (floor (* 23.0d0 s))))
	    (setq nu (+ 9 (floor (* 21.0d0 s)))))
	   (t (setq h 0.0d0) (setq capn 0) (setq nu 8)))
     (and (> h 0.0d0) (setq lamb (expt h2 capn)))
     (setq bool (or (zerop h) (zerop lamb)))
     (do ((n nu (1- n)))
	 ((> 0 n))
       (setq np1 (1+ n))
       (setq t1 (+ h (* (float np1) r1) y))
       (setq t2 (- x (* (float np1) r2)))
       (setq c (/ 0.5d0 (+ (* t1 t1) (* t2 t2))))
       (setq r1 (* c t1) r2 (* c t2))
       (cond ((and (> h 0.0) (not (< capn n)))
	      (setq t1 (+ s1 lamb) s1 (- (* r1 t1) (* r2 s2)))
	      (setq s2 (+ (* r1 s2) (* r2 t1)) lamb (/ lamb h2)))))
     (setq im (cond ((zerop y) (* 1.77245384d0 (exp (- (* x x)))))
		    (t (* 2.0d0 (cond (bool r1) (t s1))))))
     (setq re (* -2.0d0 (cond (bool r2) (t s2))))
     (cond ((> ys 0.0d0) (setq re (* re xs)))
	   (t (setq r1 (* 3.5449077 (exp (- (* y y) (* x x)))))
	      (setq r2 (* 2.0 x y))
	      (setq re (* (- re (* r1 (sin r2))) xs))
	      (setq im (- (* r1 (cos r2)) im))))
     (list '(mlist simp) re im))
   (cond ((> 0.0d0 x) -1.0d0) (t 1.0d0))
   (cond ((> 0.0d0 x) -1.0d0) (t 1.0d0))
   0 0 0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 nil 0.0d0 0.0d0)) 

(defun $nzeta ($z) 
  (prog ($x $y $w) 
     (cond ((and (numberp (setq $x ($realpart $z)))
		 (numberp (setq $y ($imagpart $z))))
	    (setq $w (z-function (float $x) (float $y)))
	    (return (simplify (list '(mplus)
				    (simplify (list '(mtimes)
						    (meval1 '$%i)
						    (caddr $w)))
				    (cadr $w)))))
	   (t (return (list '($nzeta simp) $z))))))


(defun $nzetar ($z)
  (prog ($x $y $w) 
     (cond ((and (numberp (setq $x ($realpart $z)))
		 (numberp (setq $y ($imagpart $z))))
	    (setq $w (z-function (float $x) (float $y)))
	    (return (cadr $w)))
	   (t (return (list '($nzetar simp) $z))))))


(defun $nzetai ($z)
  (prog ($x $y $w) 
     (cond ((and (numberp (setq $x ($realpart $z)))
		 (numberp (setq $y ($imagpart $z))))
	    (setq $w (z-function (float $x) (float $y)))
	    (return (caddr $w)))
	   (t (return (list '($nzetai simp) $z))))))


(defmspec $gauss (form)
  (format t
"NOTE: The gauss function is superseded by random_normal in the `distrib' package.
Perhaps you meant to enter `~a'.~%"
    (print-invert-case (implode (mstring `(($random_normal) ,@ (cdr form))))))
  '$done)

;; I think this is the function E1(x).  At least some simple numerical
;; tests show that this expint matches the function de1 from SLATEC

;; Exponential integral E1(x).  The Cauchy principal value is used for
;; negative x.
(defun $expint (x)
  (cond ((numberp x)
	 (values (slatec:de1 (float x))))
	(t
	 (list '($expint simp) x))))


;; Define the Bessel funtion J[n](z)

(defprop %bessel_j bessel-j-simp operators)

;; Derivatives of the Bessel function.
(defprop %bessel_j
    ((n x)
     ;; Derivative wrt to order n.  A&S 9.1.64.  Do we really want to
     ;; do this?  It's quite messy.
     ;;
     ;; J[n](x)*log(x/2) - (x/2)^n*sum((-1)^k*psi(n+k+1)/gamma(n+k+1)*(z^2/4)^k/k!,k,0,inf)
     ((mplus)
      ((mtimes simp)
       ((%bessel_j simp) n x)
       ((%log) ((mtimes) ((rat simp) 1 2) x)))
      ((mtimes simp) -1
       ((mexpt simp) ((mtimes simp) x ((rat simp) 1 2)) n)
       ((%sum simp)
	((mtimes simp) ((mexpt simp) -1 $%k)
	 ((mexpt simp) ((mfactorial simp) $%k) -1)
	 ((mqapply simp) (($psi simp array) 0) ((mplus simp) 1 $%k n))
	 ((mexpt simp) ((%gamma simp) ((mplus simp) 1 $%k n)) -1)
	 ((mexpt simp) ((mtimes simp) x x ((rat simp) 1 4)) $%k))
	$%k 0 $inf)))
      
     ;; Derivative wrt to arg x.  A&S 9.1.27; changed from 9.1.30 so that taylor works on Bessel functions
     ((mtimes) ((mplus) ((%bessel_j) ((mplus) -1 n) x) ((mtimes) -1 ((%bessel_j) ((mplus) 1 n) x))) ((rat) 1 2)))
;;     ((mplus)
;;      ((%bessel_j) ((mplus) -1 n) x)
;;      ((mtimes) -1 n ((%bessel_j) n x) ((mexpt) x -1))))
  grad)

;; If E is a maxima ratio with a denominator of DEN, return the ratio
;; as a Lisp rational.  Otherwise NIL.
(defun max-numeric-ratio-p (e den)
  (if (and (listp e)
	   (eq 'rat (caar e))
	   (= den (third e))
	   (integerp (second e)))
      (/ (second e) (third e))
      nil))

;; Compute the Bessel function of half-integral order.
;;
;; From A&S 10.1.1, we have
;;
;; J[n+1/2](z) = sqrt(2*z/pi)*j[n](z)
;; Y[n+1/2](z) = sqrt(2*z/pi)*y[n](z)
;;
;; where j[n](z) is the spherical bessel function of the first kind
;; and y[n](z) is the spherical bessel function of the second kind.
;;
;; A&S 10.1.8 and 10.1.9 give
;;
;; j[n](z) = 1/z*[P(n+1/2,z)*sin(z-n*pi/2) + Q(n+1/2)*cos(z-n*pi/2)]
;;
;; y[n](z) = (-1)^(n+1)*1/z*[P(n+1/2,z)*cos(z+n*pi/2) - Q(n+1/2)*sin(z+n*pi/2)]
;;

;; A&S 10.1.10
;;
;; j[n](z) = f[n](z)*sin(z) + (-1)^n*f[-n-1](z)*cos(z)
;;
;; f[0](z) = 1/z, f[1](z) = 1/z^2
;;
;; f[n-1](z) + f[n+1](z) = (2*n+1)/z*f[n](z)
;;
(defun f-fun (n z)
  (cond ((= n 0)
	 (div 1 z))
	((= n 1)
	 (div 1 (mul z z)))
	((= n -1)
	 0)
	((>= n 2)
	 ;; f[n+1](z) = (2*n+1)/z*f[n](z) - f[n-1](z) or
	 ;; f[n](z) = (2*n-1)/z*f[n-1](z) - f[n-2](z)
	 (sub (mul (div (+ n n -1) z)
		   (f-fun (1- n) z))
	      (f-fun (- n 2) z)))
	(t
	 ;; Negative n
	 ;;
	 ;; f[n-1](z) = (2*n+1)/z*f[n](z) - f[n+1](z) or
	 ;; f[n](z) = (2*n+3)/z*f[n+1](z) - f[n+2](z)
	 (sub (mul (div (+ n n 3) z)
		   (f-fun (1+ n) z))
	      (f-fun (+ n 2) z)))))

;; Compute sqrt(2*z/%pi)
(defun root-2z/pi (z)
  (let ((half (div 1 2)))
    (simplify (power (mul 2 z (inv '$%pi)) half))))

(defun bessel-j-half-order (order arg)
  "Compute J[n+1/2](z)"
  (let* ((n (floor order))
	 (sign (if (oddp n) -1 1))
	 (jn (sub (mul ($expand (f-fun n arg))
		       `((%sin) ,arg))
		  (mul sign
		       ($expand (f-fun (- (- n) 1) arg))
		       `((%cos) ,arg)))))
    (mul (root-2z/pi arg)
	 jn)))

(defun bessel-y-half-order (order arg)
  "Compute Y[n+1/2](z)"
  ;; A&S 10.1.1:
  ;; Y[n+1/2](z) = sqrt(2*z/%pi)*y[n](z)
  ;;
  ;; A&S 10.1.15:
  ;; y[n](z) = (-1)^(n+1)*j[-n-1](z)
  ;;
  ;; So
  ;; Y[n+1/2](z) = sqrt(2*z/%pi)*(-1)^(n+1)*j[-n-1](z)
  ;;             = (-1)^(n+1)*sqrt(2*z/%pi)*j[-n-1](z)
  ;;             = (-1)^(n+1)*J[-n-1/2](z)
  (let* ((n (floor order))
	 (jn (bessel-j-half-order (- (- order) 1/2) arg)))
    (if (evenp n)
	(mul -1 jn)
	jn)))
	

;; See A&S 10.2.12
;;
;; I[n+1/2](z) = sqrt(2*z/%pi)*[g[n](z)*sinh(z) + g[n-1](z)*cosh(z)]
;;
;; g[0](z) = 1/z, g[1](z) = -1/z^2
;;
;; g[n-1](z) - g[n+1](z) = (2*n+1)/z*g[n](z)
;;
;;
(defun g-fun (n z)
  (declare (type integer n))
  (cond ((= n 0)
	 (div 1 z))
	((= n 1)
	 (div -1 (mul z z)))
	((>= n 2)
	 ;; g[n](z) = g[n-2](z) - (2*n-1)/z*g[n-1](z)
	 (sub (g-fun (- n 2) z)
	      (mul (div (+ n n -1) z)
		   (g-fun (- n 1) z))))
	(t
	 ;; n is negative
	 ;;
	 ;; g[n](z) = (2*n+3)/z*g[n+1](z) + g[n+2](z)
	 (add (mul (div (+ n n 3) z)
		   (g-fun (+ n 1) z))
	      (g-fun (+ n 2) z)))))

;; See A&S 10.2.12
;;
;; I[n+1/2](z) = sqrt(2*z/%pi)*[g[n](z)*sinh(z) + g[-n-1](z)*cosh(z)]

(defun bessel-i-half-order (order arg)
  (let ((order (floor order)))
    (mul (root-2z/pi arg)
	 (add (mul ($expand (g-fun order arg))
		   `((%sinh) ,arg))
	      (mul ($expand (g-fun (- (+ order 1)) arg))
		   `((%cosh) ,arg))))))

;; See A&S 10.2.15
;;
;; sqrt(%pi/2/z)*K[n+1/2](z) = (%pi/2/z)*exp(-z)*sum (n+1/2,k)/(2*z)^k
;;
;; or
;;                                n
;; K[n+1/2](z) = sqrt(%pi/(2*z)) sum (n+1/2,k)/(2*z)^k
;;                               k=0
;;
;; where (A&S 10.1.9)
;;
;; (n+1/2,k) = (n+k)!/k!/(n-k)!
;;

(defun k-fun (n z)
  (declare (type unsigned-byte n))
  ;; Computes the sum above
  (let ((sum 1)
	(term 1))
    (loop for k from 0 upto n do
	  (setf term (mul term
			  (div (div (* (- n k) (+ n k 1))
				    (+ k 1))
			       (mul 2 z))))
	  (setf sum (add sum term)))
    sum))

(defun bessel-k-half-order (order arg)
  (let ((order (truncate (abs order))))
    (mul (mul `((mexpt) ,(div '$%pi (mul 2 arg)) ,(div 1 2))
	      `((mexpt) $%e ,(neg arg)))
	 (k-fun (abs order) arg))))

(defun bessel-numerical-eval-p (order arg)
  ;; Return non-NIL if we should numerically evaluate a bessel
  ;; function.  Basically, both args have to be numbers.  If both args
  ;; are integers, we don't evaluate unless $numer is true.
  (or (and (numberp order) (complex-number-p arg)
	   (or (floatp order) (floatp ($realpart arg)) (floatp ($imagpart arg))))
      (and $numer (numberp order)
	   (complex-number-p arg))))
	 
(defun bessel-j-simp (exp ignored z)
  (declare (ignore ignored))
  (twoargcheck exp)
  (let ((order (simpcheck (cadr exp) z))
	(rat-order nil))
    (let* ((arg (simpcheck (caddr exp) z)))
      (when (and (numberp arg) (zerop arg)
		 (numberp order))
	;; J[v](0) = 1 if v = 0.  Otherwise 0.
	(return-from bessel-j-simp
	  (if (zerop order)
	      1
	      0)))
      (cond ((bessel-numerical-eval-p order arg)
	     ;; We have numeric order and arg and $numer is true, or
	     ;; we have either the order or arg being floating-point,
	     ;; so let's evaluate it numerically.
	     ($bessel arg order))
	    ((and (integerp order) (minusp order))
	     ;; Some special cases when the order is an integer
	     ;;
	     ;; A&S 9.1.5
	     ;; J[-n](x) = (-1)^n*J[n](x)
	     (if (evenp order)
		 (list '(%bessel_j simp) (- order) arg)
		 `((mtimes simp) -1 ((%bessel_j simp) ,(- order) ,arg))))
	    ((and $besselexpand (setq rat-order (max-numeric-ratio-p order 2)))
	     ;; When order is a fraction with a denominator of 2, we
	     ;; can express the result in terms of elementary
	     ;; functions.
	     ;;
	     (bessel-j-half-order rat-order arg)
	     )
	    (t
	     (eqtest (list '(%bessel_j) order arg)
		     exp))))))


;; Define the Bessel funtion Y[n](z)

(defmfun $bessel_y (v z)
  (simplify (list '(%bessel_y) (resimplify v) (resimplify z))))

(defprop %bessel_y bessel-y-simp operators)

(defprop %bessel_y
    ((n x)
     ;; A&S 9.1.65
     ;;
     ;; cot(n*%pi)*[diff(bessel_j(n,x),n)-%pi*bessel_y(n,x)]
     ;;  - csc(n*%pi)*diff(bessel_j(-n,x),n)-%pi*bessel_j(n,x)
     ((mplus simp)
      ((mtimes simp) $%pi ((%bessel_j simp) n x))
      ((mtimes simp)
       -1
       ((%csc simp) ((mtimes simp) $%pi n))
       ((%derivative simp) ((%bessel_j simp) ((mtimes simp) -1 n) x) x 1))
      ((mtimes simp)
       ((%cot simp) ((mtimes simp) $%pi n))
       ((mplus simp)
	((mtimes simp) -1 $%pi ((%bessel_y simp) n x))
	((%derivative simp) ((%bessel_j simp) n x) n 1))))

     ;; Derivative wrt to arg x.  A&S 9.1.27; changed from A&S 9.1.30 to be consistent with bessel_j.
     ((mtimes) ((mplus) ((%bessel_y)((mplus) -1 n) x) ((mtimes) -1 ((%bessel_y) ((mplus) 1 n) x))) ((rat) 1 2)))
    ;;((mplus)
    ;; ((%bessel_y) ((mplus) -1 n) x)
    ;; ((mtimes) -1 n ((%bessel_y) n x) ((mexpt) x -1))))
    grad)

(defun bessel-y-simp (exp ignored z)
  (declare (ignore ignored))
  (twoargcheck exp)
  (let ((order (simpcheck (cadr exp) z))
	(rat-order nil))
    (let* ((arg (simpcheck (caddr exp) z)))
      (cond ((eql arg 0) (domain-error arg 'bessel_y))
	    ((and (>= (signum1 order) 0) (bessel-numerical-eval-p order arg))
	     ;; We have numeric order and arg and $numer is true, or
	     ;; we have either the order or arg being floating-point,
	     ;; so let's evaluate it numerically.
	     (bessel-y (float order) (complex ($realpart arg) ($imagpart arg))))

	  	    
	    ((and (integerp order) (minusp order))
	     ;; Special case when the order is an integer.
	     ;;
	     ;; A&S 9.1.5
	     ;; Y[-n](x) = (-1)^n*Y[n](x)
	     (if (evenp order)
		 (list '(%bessel_y) (- order) arg)
		 `((mtimes simp) -1 ((%bessel_y simp) ,(- order) ,arg))))
	    ((and $besselexpand (setq rat-order (max-numeric-ratio-p order 2)))
	     ;; When order is a fraction with a denominator of 2, we
	     ;; can express the result in terms of elementary
	     ;; functions.
	     ;;
	     ;; Y[1/2](z) = -J[1/2](z) is a function of sin.
	     ;; Y[-1/2](z) = -J[-1/2](z) is a function of cos.

	     (bessel-y-half-order rat-order arg))
	    (t
	     (eqtest (list '(%bessel_y) order arg)
		     exp))))))

;; Define the Bessel funtion I[n](z)

(defmfun $bessel_i (v z)
  (simplify (list '(%bessel_i) (resimplify v) (resimplify z))))

(defprop %bessel_i bessel-i-simp operators)

(defprop %bessel_i
    ((n x)
     ;; A&S 9.6.42
     ;;
     ;; I[n](x)*log(x/2) - (x/2)^n*sum(psi(n+k+1)/gamma(n+k+1)*(z^2/4)^k/k!,k,0,inf)
     ((mplus)
      ((mtimes simp)
       ((%bessel_i simp) n x)
       ((%log) ((mtimes) ((rat simp) 1 2) x)))
      ((mtimes simp) -1
       ((mexpt simp) ((mtimes simp) x ((rat simp) 1 2)) n)
       ((%sum simp)
	((mtimes simp)
	 ((mexpt simp) ((mfactorial simp) $%k) -1)
	 ((mqapply simp) (($psi simp array) 0) ((mplus simp) 1 $%k n))
	 ((mexpt simp) ((%gamma simp) ((mplus simp) 1 $%k n)) -1)
	 ((mexpt simp) ((mtimes simp) x x ((rat simp) 1 4)) $%k))
	$%k 0 $inf)))
     ;; Derivative wrt to x.  A&S 9.6.29.
     ((mtimes)
      ((mplus) ((%bessel_i) ((mplus) -1 n) x)
               ((%bessel_i) ((mplus) 1 n) x))
      ((rat) 1 2)))
  grad)

(defun bessel-i-simp (exp ignored z)
  (declare (ignore ignored))
  (twoargcheck exp)
  (let ((order (simpcheck (cadr exp) z))
	(rat-order nil))
    (let* ((arg (simpcheck (caddr exp) z)))
      (cond ((and (numberp arg) (zerop arg)
		  (numberp order))
	     ;; I[v](0) = 1 if v = 0, Otherwise 0
	     (if (zerop order)
		 1
		 0))
	    ((and (>= (signum1 order) 0) (bessel-numerical-eval-p order arg))
	     (bessel-i (float order) (complex ($realpart arg) ($imagpart arg))))
	    ((and (integerp order) (minusp order))
	     ;; Some special cases when the order is an integer
	     ;;
	     ;; A&S 9.6.6
	     ;; I[-n](x) = I[n](x)
	     (list '(%bessel_i) (- order) arg))
	    ((and $besselexpand (setq rat-order (max-numeric-ratio-p order 2)))
	     ;; When order is a fraction with a denominator of 2, we
	     ;; can express the result in terms of elementary
	     ;; functions.
	     ;;
	     ;; I[1/2](z) = sqrt(2/%pi/z)*sinh(z)
	     ;; I[-1/2](z) = sqrt(2/%pi/z)*cosh(z)
	     (bessel-i-half-order rat-order arg))
	    (t
	     (eqtest (list '(%bessel_i) order arg)
		     exp))))))

;; Define the Bessel funtion K[n](z)

(defmfun $bessel_k (v z)
  (simplify (list '(%bessel_k) (resimplify v) (resimplify z))))


(defprop %bessel_k bessel-k-simp operators)

(defprop %bessel_k
    ((n x)
     ;; A&S 9.6.43
     ;;
     ;; %pi/2*csc(n*%pi)*['diff(bessel_i(-n,x),n)-'diff(bessel_i(n,x),n)]
     ;;    - %pi*cot(n*%pi)*bessel_k(n,x)
     ((mplus simp)
      ((mtimes simp) -1 $%pi
       ((%bessel_k simp) n x)
       ((%cot simp) ((mtimes simp) $%pi n)))
      ((mtimes simp)
       ((rat simp) 1 2)
       $%pi
       ((%csc simp) ((mtimes simp) $%pi n))
       ((mplus simp)
	((%derivative simp) ((%bessel_i simp) ((mtimes simp) -1 n) x) n 1)
	((mtimes simp) -1
	 ((%derivative simp) ((%bessel_i simp) n x) n 1)))))
     ;; Derivative wrt to x.  A&S 9.6.29.
     ((mtimes)
      ((mplus) ((%bessel_k) ((mplus) -1 n) x)
               ((%bessel_k) ((mplus) 1 n) x))
      ((rat) 1 2)))
  grad)

(defun bessel-k-simp (exp ignored z)
  (declare (ignore ignored))
  (let ((order (simpcheck (cadr exp) z))
	(rat-order nil))
    (let* ((arg (simpcheck (caddr exp) z)))
      (cond ((and (>= (signum1 order) 0) (bessel-numerical-eval-p order arg))
	     ;; A&S 9.6.6
	     ;; K[-v](x) = K[v](x)
	     (bessel-k (abs (float order)) (complex ($realpart arg) ($imagpart arg))))
	    ((mminusp order)
	     ;; A&S 9.6.6
	     ;; K[-v](x) = K[v](x)
	     (resimplify (list '(%bessel_k) `((mtimes) -1 ,order) arg)))
	    ((and $besselexpand
		  (setq rat-order (max-numeric-ratio-p order 2)))
	     ;; When order is a fraction with a denominator of 2, we
	     ;; can express the result in terms of elementary
	     ;; functions.
	     ;;
	     ;; K[1/2](z) = sqrt(2/%pi/z)*exp(-z) = K[1/2](z)
	     (bessel-k-half-order rat-order arg))
	    (t
	     (eqtest (list '(%bessel_k) order arg)
		     exp))))))
