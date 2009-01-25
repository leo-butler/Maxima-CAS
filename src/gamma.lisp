;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Double Factorial, Incomplete Gamma function, ...
;;;
;;; This file will be extended with further functions related to the 
;;; Factorial and Gamma functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file containts the following Maxima User functions:
;;;
;;;   double_factorial(z)
;;;
;;;   gamma_incomplete(a,z)
;;;   gamma_incomplete_generalized(a,z1,z2)
;;;   gamma_incomplete_regularized(a,z)
;;;
;;;   log_gamma(z)
;;;
;;;   erf(z)
;;;   erfc(z)
;;;   erfi(z)
;;;   erf_generalized(z1,z2)
;;;
;;;   fresnel_s(z)
;;;   fresnel_c(z)
;;;
;;;   beta_incomplete(a,b,z)
;;;
;;; Maxima User variable:
;;;
;;;   $factorial_expand    - Allows argument simplificaton for expressions like
;;;                          factorial_double(n-1) and factorial_double(2*k+n)
;;;   $erf_representation  - When T erfc, erfi, erf_generalized, fresnel_s 
;;;                          and fresnel_c are transformed to erf.
;;;   $erf_%iargs          - Enable simplification of Erf and Erfi for
;;;                          imaginary arguments
;;;   $hypergeometric_representation
;;;                        - Enables transformation to a Hypergeometric
;;;                          representation for fresnel_s and fresnel_c
;;;
;;; Maxima User variable (not definied in this file):
;;;
;;;   $factlim             - biggest integer for numerically evaluation
;;;                          of the Double factorial
;;;   $gamma_expand        - Expansion of the Gamma und Incomplete Gamma
;;;                          function for some special cases
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the 
;;; Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along 
;;; with this library; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Copyright (C) 2008 Dieter Kaiser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(declare-top (special $factlim $gamma_expand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar $factorial_expand nil)

(defvar $erf_representation nil
  "When T erfc, erfi and erf_generalized are transformed to erf.")

(defvar $erf_%iargs nil
  "When T erf and erfi simplifies for an imaginary argument.")

(defvar $hypergeometric_representation nil
  "When T a transformation to a hypergeometric representation is done.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following functions test if numerical evaluation has to be done.
;;; The functions should help to test for numerical evaluation more consitent
;;; and without complicated conditional tests including more than one or two
;;; arguments.
;;;
;;; The functions take a list of arguments. All arguments have to be a CL or
;;; Maxima number. If all arguments are numbers we have two cases:
;;; 1. $numer is T we return T. The function has to be evaluated numerically.
;;; 2. One of the args is a float or a bigfloat. Evaluate numerically.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test for numerically evaluation in float precision

(defun float-numerical-eval-p (&rest args)
  (let ((flag nil))
    (dolist (ll args)
      (when (not (float-or-rational-p ll)) 
        (return-from float-numerical-eval-p nil))
      (when (floatp ll) (setq flag t)))
    (if (or $numer flag) t nil)))

;;; Test for numerically evaluation in complex float precision

(defun complex-float-numerical-eval-p (&rest args)
  (let ((flag nil))
    (dolist (ll args)
      (when (not (complex-number-p ll 'float-or-rational-p)) 
        (return-from complex-float-numerical-eval-p nil))
      (when (or (floatp ($realpart ll)) (floatp ($imagpart ll)))
        (setq flag t)))
    (if (or $numer flag) t nil)))

;;; Test for numerically evaluation in bigfloat precision

(defun bigfloat-numerical-eval-p (&rest args)
  (let ((flag nil))
    (dolist (ll args)
      (when (not (bigfloat-or-number-p ll)) 
        (return-from bigfloat-numerical-eval-p nil))
      (when ($bfloatp ll) (setq flag t)))
    (if (or $numer flag) t nil)))

;;; Test for numerically evaluation in complex bigfloat precision

(defun complex-bigfloat-numerical-eval-p (&rest args)
  (let ((flag nil))
    (dolist (ll args)
      (when (not (complex-number-p ll 'bigfloat-or-number-p)) 
        (return-from complex-bigfloat-numerical-eval-p nil))
      (when (or ($bfloatp ($realpart ll)) ($bfloatp ($imagpart ll)))
        (setq flag t)))
    (if (or $numer flag) t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The changes to the parser to connect the operator !! to double_factorial(z)

;(def-mheader |$!!| (%double_factorial))

;(def-led (|$!!| 160.) (op left)
;  (list '$expr
;	(mheader '$!!)
;	(convert left '$expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The implementation of the function Double factorial
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $double_factorial (z)
  (simplify (list '(%double_factorial) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $double_factorial %double_factorial alias)
(defprop $double_factorial %double_factorial verb)

(defprop %double_factorial $double_factorial reversealias)
(defprop %double_factorial $double_factorial noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Double factorial is a simplifying function

(defprop %double_factorial simp-double-factorial operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Double factorial has mirror symmetry

(defprop %double_factorial t commutes-with-conjugate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Differentiation of Double factorial

(defprop %double_factorial
  ((z)
   ((mtimes) 
      ((rat) 1 2)
      ((%double_factorial) z)
      ((mplus) 
         ((%log) 2)
         ((mqapply) 
            (($psi array) 0)
            ((mplus) 1 ((mtimes) ((rat) 1 2) z)))
         ((mtimes) 
            ((rat) 1 2) $%pi
            ((%log) ((mtimes) 2 ((mexpt) $%pi -1)))
            ((%sin) ((mtimes) $%pi z))))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-double-factorial (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond    
    ((and (fixnump z) (> z -1) (or (minusp $factlim) (< z $factlim)))
     ;; Positive Integer less then $factlim or $factlim is -1. Call gfact.
     (gfact z (floor (/ z 2)) 2))

    ((and (mnump z)
          (eq ($sign z) '$neg)          
          (zerop1 (sub (simplify (list '(%truncate) (div z 2))) (div z 2))))
     ;; Even negative integer or real representation. Not defined.
     (merror "double_factorial(~:M) is undefined." z))

    ((or (integerp z)   ; at this point odd negative integer. Evaluate.
         (complex-float-numerical-eval-p z))
     (cond
       ((and (integerp z) (= z -1))  1)  ; Special cases -1 and -3 
       ((and (integerp z) (= z -3)) -1)
       (t
        ;; Odd negative integer, float or complex float.
        (complexify 
          (double-factorial 
            (complex ($float ($realpart z)) ($float ($imagpart z))))))))
  
    ((and (not (ratnump z))
          (complex-bigfloat-numerical-eval-p z))
     ;; bigfloat or complex bigfloat.
     (bfloat-double-factorial 
       (add ($bfloat ($realpart z)) (mul '$%i ($bfloat ($imagpart z))))))

    ;; double_factorial(inf) -> inf
    ((eq z '$inf) '$inf)

    ((and $factorial_expand
          (mplusp z)
          (integerp (cadr z)))
     (let ((k (cadr z))
           (n (simplify (cons '(mplus) (cddr z)))))
       (cond
         ((= k -1)
          ;; Special case double_factorial(n-1)
          ;; Not sure if this simplification is useful.
          (div (simplify (list '(mfactorial) n)) 
               (simplify (list '(%double_factorial) n))))
         ((= k (* 2 (truncate (/ k 2))))
          ;; Special case double_factorial(2*k+n), k integer
          (setq k (/ k 2))
          ($factor   ; we get more simple expression when factoring
            (mul
              (power 2 k)
              (simplify (list '($pochhammer) (add (div n 2) 1) k))
              (simplify (list '(%double_factorial) n)))))
         (t
           (eqtest (list '(%double_factorial) z) expr)))))

    (t
      (eqtest (list '(%double_factorial) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Double factorial for a complex float argument. The result is a CL complex.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun double-factorial (z)
  (let ((pival (float pi)))
    (*
     (expt
      (/ 2 pival)
      (/ (- 1 (cos (* pival z))) 4))
     (expt 2d0 (/ z 2))
     (gamma-lanczos (+ 1 (/ z 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Double factorial for a bigfloat or complex bigfloat argument
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfloat-double-factorial (z)
  (let* ((pival ($bfloat '$%pi))
         (bigfloat1 ($bfloat bigfloatone))
         (bigfloat2 (add bigfloat1 bigfloat1))
         (bigfloat4 (add bigfloat2 bigfloat2))
         ($ratprint nil))
    (cmul
      (cpower
        (cdiv bigfloat2 pival)
        (cdiv (sub bigfloat1 
                   (simplify (list '(%cos) (cmul pival z)))) bigfloat4))
      (cmul
        (cpower bigfloat2 (cdiv z bigfloat2))
        (simplify (list '(%gamma) (add bigfloat1 (cdiv z bigfloat2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The implementation of the Incomplete Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debug-gamma* nil)

(defun $gamma_incomplete (a z)
  (simplify (list '(%gamma_incomplete) (resimplify a) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $gamma_incomplete %gamma_incomplete alias)
(defprop $gamma_incomplete %gamma_incomplete verb)

(defprop %gamma_incomplete $gamma_incomplete reversealias)
(defprop %gamma_incomplete $gamma_incomplete noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Incomplete Gamma function is a simplifying function

(defprop %gamma_incomplete simp-gamma-incomplete operators)

;;; Incomplete Gamma function has not mirror symmetry for z on the negative
;;; real axis. We support a conjugate-function which test this case.

(defprop %gamma_incomplete conjugate-gamma-incomplete conjugate-function)

(defun conjugate-gamma-incomplete (args)
  (let ((a (first args)) (z (second args)))
    (cond ((off-negative-real-axisp z)
           ;; Definitly not on the negative real axis for z. Mirror symmetry.
	   (simplify
             (list
              '(%gamma_incomplete)
               (simplify (list '($conjugate) a))
               (simplify (list '($conjugate) z)))))
	  (t
           ;; On the negative real axis or no information. Unsimplified.
           (list
            '($conjugate simp)
             (simplify (list '(%gamma_incomplete) a z)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Derivative of the Incomplete Gamma function

(defprop %gamma_incomplete
  ((a z)
   ;; The derivative wrt a in terms of hypergeometric_generalized 2F2 function
   ;; and the Generalized Incomplete Gamma function (functions.wolfram.com)
   ((mplus)
      ((mtimes)
         ((mexpt) ((%gamma) a) 2)
         ((mexpt) z a)
         (($hypergeometric_generalized)
            ((mlist) a a)
            ((mlist) ((mplus) 1 a) ((mplus) 1 a))
            ((mtimes) -1 z)))
      ((mtimes) -1
         ((%gamma_incomplete_generalized) a 0 z)
         ((%log) z))
      ((mtimes)
         ((%gamma) a)
         ((mqapply) (($psi array) 0) a)))
   ;; The derivative wrt z
   ((mtimes) -1
      ((mexpt) $%e ((mtimes) -1 z))
      ((mexpt) z ((mplus) -1 a))))
  grad)

;;; Integral of the Incomplete Gamma function

(defprop %gamma_incomplete
  ((a z)
   nil
   ((mplus)
      ((mtimes) -1 ((%gamma_incomplete) ((mplus) 1 a) z))
      ((mtimes) ((%gamma_incomplete) a z) z)))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-gamma-incomplete (expr ignored simpflag)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((a (simpcheck (cadr expr) simpflag))
        (z (simpcheck (caddr expr) simpflag))
        (ratorder))
    (cond

      ;; Check for specific values

      ((zerop1 z)
       (let ((sgn ($sign ($realpart a))))
         (cond ((eq sgn '$neg) (domain-error 0 'gamma_incomplete))
               ((eq sgn '$zero) (domain-error 0 'gamma_incomplete))
               ((member sgn '($pos $pz)) ($gamma a))
               (t (eqtest (list '(%gamma_incomplete) a z) expr)))))
              
      ((eq z '$inf) 0)

      ;; Check for numerical evaluation in Float or Bigfloat precision

      ((float-numerical-eval-p a z)
       (when *debug-gamma* 
         (format t "~&SIMP-GAMMA-INCOMPLETE in float-numerical-eval-p~%"))
       ;; a and z are Maxima numbers, at least one has a float value
       (let ((a ($float a))
             (z ($float z)))
         (cond
           ((or (= a 0.0)
                (and (= 0 (- a (truncate a)))
                     (< a 0.0)))
            ;; a is zero or a negative float representing an integer.
            ;; For these cases the numerical routines of gamma-incomplete
            ;; do not work. Call the numerical routine for the Exponential 
            ;; Integral E(n,z). The routine is called with a positive integer!.
            (setq a (truncate a))
            (complexify (* (expt z a) (expintegral-e (- 1 a) z))))
           (t
             (complexify (gamma-incomplete a z))))))

      ((complex-float-numerical-eval-p a z)
       (when *debug-gamma* 
         (format t 
           "~&SIMP-GAMMA-INCOMPLETE in complex-float-numerical-eval-p~%"))
       ;; a and z are Maxima numbers, at least one is a complex value and
       ;; we have at least one float part
       (let ((ca (complex ($float ($realpart a)) ($float ($imagpart a))))
             (cz (complex ($float ($realpart z)) ($float ($imagpart z)))))
         (cond
           ((and (= (imagpart ca) 0.0)
                 (or (= (realpart ca) 0.0)
                     (and (= 0 (- (realpart ca) (truncate (realpart ca))))
                          (< (realpart ca) 0.0))))
            ;; Call expintegral-e. See comment above.
            (setq ca (truncate (realpart ca)))
            (complexify (* (expt cz ca) (expintegral-e (- 1 ca) cz))))
           (t
            (complexify (gamma-incomplete ca cz))))))
           
      ((bigfloat-numerical-eval-p a z)
       (when *debug-gamma* 
         (format t "~&SIMP-GAMMA-INCOMPLETE in bigfloat-numerical-eval-p~%"))
       (let ((a ($bfloat a))
             (z ($bfloat z)))
         (cond         
           ((or (eq ($sign a) '$zero)
                (and (eq ($sign (sub a ($truncate a))) '$zero)
                     (eq ($sign a) '$neg)))
            ;; Call bfloat-expintegral-e. See comment above.
            (setq a ($truncate a))
            ($rectform (mul (power z a) (bfloat-expintegral-e (- 1 a) z))))
           (t
             (bfloat-gamma-incomplete a z)))))

      ((complex-bigfloat-numerical-eval-p a z)
       (when *debug-gamma* 
         (format t 
           "~&SIMP-GAMMA-INCOMPLETE in complex-bigfloat-numerical-eval-p~%"))
       (let ((ca (add ($bfloat ($realpart a))
                      (mul '$%i ($bfloat ($imagpart a)))))
             (cz (add ($bfloat ($realpart z)) 
                      (mul '$%i ($bfloat ($imagpart z))))))
         (cond         
           ((and (eq ($sign ($imagpart ca)) '$zero)
                 (or (eq ($sign ($realpart ca)) '$zero)
                     (and (eq ($sign (sub ($realpart ca)
                                          ($truncate ($realpart ca))))
                              '$zero)
                          (eq ($sign ($realpart ca)) '$neg))))
            ;; Call bfloat-expintegral-e. See comment above.
            (when *debug-gamma* 
               (format t 
                 "~& bigfloat-numerical-eval-p calls bfloat-expintegral-e~%"))
            (setq a ($truncate ($realpart a)))
            ($rectform (mul (power cz a)
                            (bfloat-expintegral-e (- 1 a) cz))))
           (t
            (complex-bfloat-gamma-incomplete ca cz)))))

      ;; Check for transformations and argument simplification

      ((and $gamma_expand (integerp a))
       ;; Integer or a symbol declared to be an integer. Expand in a series.
       (let ((sgn ($sign a)))
         (cond
           ((eq sgn '$zero)
            (add
              (mul -1
                (simplify (list '(%expintegral_ei) (mul -1 z))))
              (mul
                (div 1 2)
                (sub
                  (simplify (list '(%log) (mul -1 z)))
                  (simplify (list '(%log) (div -1 z)))))
              (mul -1 (simplify (list '(%log) z)))))
           ((member sgn '($pos $pz))
            (mul
              (simplify (list '(%gamma) a))
              (power '$%e (mul -1 z))
              (let ((index (gensumindex)))
                (dosum
                  (div 
                    (power z index)
                    (list '(%gamma) (add index 1)))
                  index 0 (sub a 1) t))))
           ((member sgn '($neg $nz))
            (sub
              (mul
                (div
                  (power -1 (add (mul -1 a) -1))
                  (simplify (list '(%gamma) (add (mul -1 a) 1))))
                (add
                  (simplify (list '(%expintegral_ei) (mul -1 z)))
                  (mul
                    (div -1 2)
                    (sub
                      (simplify (list '(%log) (mul -1 z)))
                      (simplify (list '(%log) (div -1 z)))))
                  (simplify (list '(%log) z))))
              (mul
                (power '$%e (mul -1 z))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z (add index a -1))
                      (simplify (list '($pochhammer) a index)))
                    index 1 (mul -1 a) t)))))
           (t (eqtest (list '(%gamma_incomplete) a z) expr)))))

      ((and $gamma_expand (setq ratorder (max-numeric-ratio-p a 2)))
       ;; We have a half integral order and $gamma_expand is not NIL.
       ;; We expand in a series with the Erfc function
       (setq ratorder (- ratorder (/ 1 2)))
       (cond
         ((equal ratorder 0)
          (mul 
            (power '$%pi '((rat) 1 2))
            (simplify (list '(%erfc) (power z '((rat) 1 2))))))
         ((> ratorder 0)
          (sub
            (mul
              (simplify (list '(%gamma) a))
              (simplify (list '(%erfc) (power z '((rat) 1 2)))))
            (mul
              (power -1 (sub ratorder 1))
              (power '$%e (mul -1 z))
              (power z '((rat) 1 2))
              (let ((index (gensumindex)))
                (dosum
                  (mul -1                      ; we get more simple results
                    (simplify                  ; when multiplying with -1  
                      (list 
                       '($pochhammer)
                        (sub (div 1 2) ratorder)
                        (sub ratorder (add index 1))))
                    (power (mul -1 z) index))
                  index 0 (sub ratorder 1) t)))))
         ((< ratorder 0)
          (setq ratorder (- ratorder))
          (sub
            (div
              (mul
                (power -1 ratorder)
                (power '$%pi '((rat) 1 2))
                (simplify (list '(%erfc) (power z '((rat) 1 2)))))
              (simplify (list '($pochhammer) (div 1 2) ratorder)))
            (mul 
              (power z (sub (div 1 2) ratorder))
              (power '$%e (mul -1 z))
              (let ((index (gensumindex)))
                (dosum
                  (div
                    (power z index)
                    (simplify 
                      (list 
                       '($pochhammer) 
                        (sub (div 1 2) ratorder)  
                        (add index 1))))
                  index 0 (sub ratorder 1) t)))))))

      ((and $gamma_expand (mplusp a) (integerp (cadr a)))
       (let ((n (cadr a))
             (a (simplify (cons '(mplus) (cddr a)))))
         (cond
           ((> n 0)
            (add
              (mul
                (simplify (list '($pochhammer) a n))
                (simplify (list '(%gamma_incomplete) a z)))
              (mul
                (power '$%e (mul -1 z))
                (power z (add a n -1))
                (let ((index (gensumindex)))
                  (dosum
                    (mul
                      (simplify 
                        (list 
                         '($pochhammer) (add 1 (mul -1 a) (mul -1 n)) index))
                      (power (mul -1 z) (mul -1 index)))
                    index 0 (add n -1) t)))))
           ((< n 0)
            (setq n (- n))
            (sub
              (div
                (mul
                  (power -1 n)
                  (simplify (list '(%gamma_incomplete) a z)))
                (simplify (list '($pochhammer) (sub 1 a) n)))
              (mul
                (power '$%e (mul -1 z))
                (power z (sub a n))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z index)
                      (simplify (list '($pochhammer) (sub a n) (add index 1))))
                    index 0 (sub n 1) t))))))))

      (t (eqtest (list '(%gamma_incomplete) a z) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical evaluation of the Incomplete Gamma function
;;;
;;;  gamma-incomplete (a,z)                - real and complex double float
;;;  bfloat-gamma-incomplete (a z)         - bigfloat
;;;  complex-bfloat-gamma-incomplete (a z) - complex bigfloat
;;;
;;;  Expansion in a power series for realpart(z) < 0 and realpart(z) > 1.0
;;;  (A&S 6.5.29):
;;;
;;;                            inf
;;;                            ===    
;;;                            \      gamma(a)
;;;  gamma(a,z) = exp(-x)*z^a * >   ------------ * z^n
;;;                            /    gamma(a+1+n)
;;;                            ===
;;;                            n=0
;;;
;;; This expansion does not work for an integer a<=0, because the Gamma function
;;; in the denominator is not defined for a=0 and negative integers. For this
;;; case we use the Exponential Integral E for numerically evaluation. The
;;; Incomplete Gamma function and the Exponential integral are connected by
;;;
;;; gamma(a,z) = z^a * expintegral_e(1-a,z)
;;;
;;; Expansion in continued fractions for realpart(z) > 1.0 (A&S 6.5.31):
;;;
;;;                              1   1-a   1   2-a   2
;;;  gamma(a,z) = exp(-x) z^a *( --  ---  ---  ---  --- ... )  
;;;                              z+  1+   z+   1+   z+ 
;;;
;;; The accuracy is controlled by *gamma-incomplete-eps* for double float
;;; precision. For bigfloat precision epsilon is 10^(-$fpprec). The expansions
;;; in a power series or continued fractions stops if *gamma-incomplete-maxit*
;;; is exceeded and an Maxima error is thrown.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *gamma-incomplete-maxit* 10000)
(defvar *gamma-incomplete-eps* 1.0e-16)
(defvar *gamma-incomplete-min* 1.0e-32)

(defvar $gamma_radius 1.0
  "Controls the radius within a series expansion is done.")
(defvar $gamma_imag 1.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The numerical evaluation for CL float or complex values a and x
;;; When the flag regularized is T, the result is divided by gamma(a) and
;;; Maxima returns the numercial result for gamma_incomplete_regularized

(defun gamma-incomplete (a x &optional (regularized nil))
  (let ((gm-maxit *gamma-incomplete-maxit*)
        (gm-eps   *gamma-incomplete-eps*)
        (gm-min   *gamma-incomplete-min*))
    (when *debug-gamma*
        (format t "~&GAMMA-INCOMPLETE with ~A and ~A~%" a x))
    (cond
      ;; The series expansion is done for x within a circle with a radius
      ;; $gamma_radius+abs(realpart(a)), for all x which are on the negative 
      ;; real axis and for all x which have an angle between [3*%pi/4,5*%pi/4] 
      ((and (> (abs x) (+ $gamma_radius 
                          (if (> (realpart a) 0.0) (realpart a) 0.0)))
            (not (and (< (realpart x) 0)
                      (< (abs (imagpart x))
                         (* $gamma_imag (abs (realpart x)))))))
       ;; Expansion in continued fractions
       (when *debug-gamma* 
         (format t "~&GAMMA-INCOMPLETE in continued fractions~%"))
       (do* ((i 1 (+ i 1))
             (an (- a 1.0) (* i (- a i)))
             (b (+ 3.0 x (- a)) (+ b 2.0))
             (c (/ 1.0 gm-min))
             (d (/ 1.0 (- b 2.0)))
             (h d)
             (del 0.0))
            ((> i gm-maxit)
             (merror "Continued fractions failed in `gamma_incomplete'"))
         (setq d (+ (* an d) b))
         (when (< (abs d) gm-min) (setq d gm-min))
         (setq c (+ b (/ an c)))
         (when (< (abs c) gm-min) (setq c gm-min))
         (setq d (/ 1.0 d))
         (setq del (* d c))
         (setq h (* h del))
         (when (< (abs (- del 1.0)) gm-eps)
           (return
             (let ((result (* h (expt x a) (exp (- x)))))
               (cond 
                 (regularized
                   ;; Return gamma_incomplete_regularized
                   (cond
                     ((complexp a)
                      (/ result (gamma-lanczos a)))
                     (t
                      ;; Call the more precise function  gammafloat 
                      (/ result (gammafloat a)))))
                 (t 
                  result)))))))

       (t
        ;; Expansion in a series
        (when *debug-gamma* 
          (format t "~&GAMMA-INCOMPLETE in series~%"))
        (do* ((i 1 (+ i 1))
              (ap a (+ ap 1.0))
              (del (/ 1.0 a) (* del (/ x ap)))
              (sum del (+ sum del)))
             ((> i gm-maxit)
              (merror "Series expansion failed in `gamma_incomplete"))
          (when (< (abs del) (* (abs sum) gm-eps))
            (when *debug-gamma* (format t "~&Series converged.~%"))
            (return
              (let ((result (* sum (expt x a) (exp (- x)))))
              (cond
                ((complexp a)
                  (cond
                    (regularized
                     ;; Return gamma_incomplete_regularized
                     (- 1.0 (/ result (gamma-lanczos a))))
                    (t 
                     (- (gamma-lanczos a) result ))))
                
                (t
                 (cond
                   (regularized
                    ;; Return gamma_incomplete_regularized
                    (- 1.0 (/ result (gammafloat a))))
                   (t
                    (- (gammafloat a) result)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This function is called for a and x real

(defun bfloat-gamma-incomplete (a x)
  (let* ((gm-maxit *gamma-incomplete-maxit*)
         (gm-eps (power ($bfloat 10.0) (- $fpprec)))
         (gm-min (mul gm-eps gm-eps))
         ($ratprint nil))
    (cond
      ((and (eq ($sign x) '$pos)
            (eq ($sign (sub (simplify (list '(mabs) x))
                            (add $gamma_radius
                                 (if (eq ($sign a) '$pos) a 0.0))))
                '$pos))
       ;; Expansion in continued fractions of the Incomplete Gamma function
       (do* ((i 1 (+ i 1))
             (an (sub a 1.0) (mul i (sub a i)))
             (b (add 3.0 x (mul -1 a)) (add b 2.0))
             (c (div 1.0 gm-min))
             (d (div 1.0 (sub b 2.0)))
             (h d)
             (del 0.0))
            ((> i gm-maxit)
             (merror "Continued fractions failed in `gamma_incomplete"))
         (when *debug-gamma* 
           (format t "~&in coninued fractions:~%")
           (format t "~&   : i = ~A~%" i)
           (format t "~&   : h = ~A~%" h))
         (setq d (add (mul an d) b))
         (when (eq ($sign (sub (simplify (list '(mabs) d)) gm-min)) '$neg)
           (setq d gm-min))
         (setq c (add b (div an c)))
         (when (eq ($sign (sub (simplify (list '(mabs) c)) gm-min)) '$neg)
           (setq c gm-min))
         (setq d (div 1.0 d))
         (setq del (mul d c))
         (setq h (mul h del))
         (when (eq ($sign (sub (simplify (list '(mabs) (sub del 1.0))) gm-eps))
                   '$neg)
           (return 
             (mul h
                  (power x a) 
                  (power ($bfloat '$%e) (mul -1 x)))))))

      (t
       ;; Series expansion of the Incomplete Gamma function
       (do* ((i 1 (+ i 1))
             (ap a (add ap 1.0))
             (del (div 1.0 a) (mul del (div x ap)))
             (sum del (add sum del)))
            ((> i gm-maxit)
             (merror "Series expansion failed in `gamma-incomplete'"))
         (when *debug-gamma* 
           (format t "~&GAMMA-INCOMPLETE in series:~%")
           (format t "~&   : i    = ~A~%" i)
           (format t "~&   : ap   = ~A~%" ap)
           (format t "~&   : x/ap = ~A~%" (div x ap))
           (format t "~&   : del  = ~A~%" del)
           (format t "~&   : sum  = ~A~%" sum))
         (when (eq ($sign (sub (simplify (list '(mabs) del)) 
                               (mul (cabs sum) gm-eps)))
                   '$neg)
           (when *debug-gamma* (format t "~&Series converged.~%"))
           (return 
             (sub (simplify (list '(%gamma) a))
                  (mul sum
                       (power x a)
                       (power ($bfloat '$%e) (mul -1 x)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun complex-bfloat-gamma-incomplete (a x)
  (let* ((gm-maxit *gamma-incomplete-maxit*)
         (gm-eps (power ($bfloat 10.0) (- $fpprec)))
         (gm-min (mul gm-eps gm-eps))
         ($ratprint nil))
    (when *debug-gamma*
      (format t "~&COMPLEX-BFLOAT-GAMMA-INCOMPLETE~%")
      (format t "   : a = ~A~%" a)
      (format t "   : x = ~A~%" x))
    (cond
      ;; The series expansion is done for x within a circle with a radius
      ;; $gamma_radius+abs(realpart(a)), for all x which are on the negative 
      ;; real axis and for all x which have an angle between [3*%pi/4,5*%pi/4]
      ((and (eq ($sign (sub (simplify (list '(mabs) x))
                            (add $gamma_radius
                                 (if (eq ($sign ($realpart a)) '$pos)
                                     ($realpart a)
                                     0.0))))
                '$pos)
            (not (and (eq ($sign ($realpart x)) '$neg)
                      (eq ($sign (sub (simplify (list '(mabs) ($imagpart x)))
                                      (simplify (list '(mabs) ($realpart x)))))
                          '$neg))))
       ;; Expansion in continued fractions of the Incomplete Gamma function
       (when *debug-gamma* 
         (format t "~&in continued fractions:~%"))
       (do* ((i 1 (+ i 1))
             (an (sub a 1.0) (mul i (sub a i)))
             (b (add 3.0 x (mul -1 a)) (add b 2.0))
             (c (cdiv 1.0 gm-min))
             (d (cdiv 1.0 (sub b 2.0)))
             (h d)
             (del 0.0))
            ((> i gm-maxit)
             (merror "Continued fractions failed in `gamma_incomplete"))
         (setq d (add (cmul an d) b))
         (when (eq ($sign (sub (simplify (list '(mabs) d)) gm-min)) '$neg)
           (setq d gm-min))
         (setq c (add b (cdiv an c)))
         (when (eq ($sign (sub (simplify (list '(mabs) c)) gm-min)) '$neg)
           (setq c gm-min))
         (setq d (cdiv 1.0 d))
         (setq del (cmul d c))
         (setq h (cmul h del))
         (when (eq ($sign (sub (simplify (list '(mabs) (sub del 1.0))) 
                               gm-eps))
                   '$neg)
           (return
             ($bfloat ; force evaluation of expressions with sin or cos
               (cmul h
                 (cmul
                   (cpower x a)
                   (cpower ($bfloat '$%e) ($bfloat (mul -1 x))))))))))

      (t
       ;; Series expansion of the Incomplete Gamma function
       (when *debug-gamma*
           (format t "~&GAMMA-INCOMPLETE in series:~%"))
       (do* ((i 1 (+ i 1))
             (ap a (add ap 1.0))
             (del (cdiv 1.0 a) (cmul del (cdiv x ap)))
             (sum del (add sum del)))
            ((> i gm-maxit)
             (merror "Series expansion failed in `gamma-incomplete'"))
         (when (eq ($sign (sub (simplify (list '(mabs) del)) 
                               (mul (cabs sum) gm-eps)))
                   '$neg)
           (when *debug-gamma* (format t "~&Series converged.~%"))
           (return
             ($bfloat ; force evaluation of expressions with sin or cos
               (sub (simplify (list '(%gamma) a))
                    (cmul sum
                      (cmul
                        (cpower x a)
                        (cpower ($bfloat '$%e) (mul -1 x)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Generalized Incomplete Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $gamma_incomplete_generalized (a z1 z2)
  (simplify (list '(%gamma_incomplete_generalized)
                   (resimplify a) (resimplify z1) (resimplify z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set the properties alias, reversealias, noun and verb

(defprop $gamma_incomplete_generalized %gamma_incomplete_generalized alias)
(defprop $gamma_incomplete_generalized %gamma_incomplete_generalized verb)

(defprop %gamma_incomplete_generalized 
         $gamma_incomplete_generalized reversealias)
(defprop %gamma_incomplete_generalized 
         $gamma_incomplete_generalized noun)

;;; Generalized Incomplete Gamma function has not mirror symmetry for z1 or z2 
;;; on the negative real axis. 
;;; We support a conjugate-function which test this case.

(defprop %gamma_incomplete_generalized 
         conjugate-gamma-incomplete-generalized conjugate-function)

(defun conjugate-gamma-incomplete-generalized (args)
  (let ((a (first args)) (z1 (second args)) (z2 (third args)))
    (cond ((and (off-negative-real-axisp z1) (off-negative-real-axisp z2))
           ;; z1 and z2 definitly not on the negative real axis. 
           ;; Mirror symmetry.
	   (simplify
             (list
              '(%gamma_incomplete_generalized)
               (simplify (list '($conjugate) a))
               (simplify (list '($conjugate) z1))
               (simplify (list '($conjugate) z2)))))
	  (t
           ;; On the negative real axis or no information. Unsimplified.
           (list
            '($conjugate simp)
             (simplify (list '(%gamma_incomplete_generalized) a z1 z2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generalized Incomplete Gamma function is a simplifying function

(defprop %gamma_incomplete_generalized 
         simp-gamma-incomplete-generalized operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Differentiation of Generalized Incomplete Gamma function

(defprop %gamma_incomplete_generalized
  ((a z1 z2)
   ;; The derivative wrt a in terms of hypergeometric_generalized 2F2 function
   ;; and the Generalized Incomplete Gamma function (functions.wolfram.com)
   ((mplus)
      ((mtimes)
         ((mexpt) ((%gamma) a) 2)
         ((mexpt) z1 a)
         (($hypergeometric_generalized)
            ((mlist) a a)
            ((mlist) ((mplus) 1 a) ((mplus) 1 a))
            ((mtimes) -1 z1)))
      ((mtimes) -1
         ((mexpt) ((%gamma) a) 2)
         ((mexpt) z2 a)
         (($hypergeometric_generalized)
            ((mlist) a a)
            ((mlist) ((mplus) 1 a) ((mplus) 1 a))
            ((mtimes) -1 z2)))
      ((mtimes) -1
         ((%gamma_incomplete_generalized) a 0 z1)
         ((%log) z1))
      ((mtimes)
         ((%gamma_incomplete_generalized) a 0 z2)
         ((%log) z2)))
   ;; The derivative wrt z1
   ((mtimes) -1
      ((mexpt) $%e ((mtimes) -1 z1))
      ((mexpt) z1 ((mplus) -1 a)))
   ;; The derivative wrt z2
   ((mtimes)
      ((mexpt) $%e ((mtimes) -1 z2))
      ((mexpt) z2 ((mplus) -1 a))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-gamma-incomplete-generalized (expr ignored simpflag)
  (declare (ignore ignored))
  (if (not (= (length expr) 4)) (wna-err '$gamma_incomplete_generalized))
  (let ((a  (simpcheck (cadr expr)   simpflag))
        (z1 (simpcheck (caddr expr)  simpflag))
        (z2 (simpcheck (cadddr expr) simpflag)))

    (cond

      ;; Check for specific values

      ((zerop1 z2)
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((eq sgn '$pos) 
            (sub
              (simplify (list '(%gamma_incomplete) a z1))
              (simplify (list '(%gamma) a))))
           (t 
            (eqtest (list '(%gamma_incomplete_generalized) a z1 z2) expr)))))

      ((zerop1 z1)
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((eq sgn '$pos) 
            (sub
              (simplify (list '(%gamma) a))
              (simplify (list '(%gamma_incomplete) a z2))))
           (t 
            (eqtest (list '(%gamma_incomplete_generalized) a z1 z2) expr)))))

      ((zerop1 (sub z1 z2)) 0)

      ((eq z2 '$inf) (simplify (list '(%gamma_incomplete) a z1)))
      ((eq z1 '$inf) (mul -1 (simplify (list '(%gamma_incomplete) a z2))))

      ;; Check for numerical evaluation in Float or Bigfloat precision
      ;; Use the numerical routines of the Incomplete Gamma function

      ((float-numerical-eval-p a z1 z2)
       (complexify 
         (- (gamma-incomplete ($float a) ($float z1)) 
            (gamma-incomplete ($float a) ($float z2)))))

      ((complex-float-numerical-eval-p a z1 z2)
       (let ((ca  (complex ($float ($realpart a))  ($float ($imagpart a))))
             (cz1 (complex ($float ($realpart z1)) ($float ($imagpart z1))))
             (cz2 (complex ($float ($realpart z2)) ($float ($imagpart z2)))))
         (complexify (- (gamma-incomplete ca cz1) (gamma-incomplete ca cz2)))))
           
      ((bigfloat-numerical-eval-p a z1 z2)
       (sub (bfloat-gamma-incomplete ($bfloat a) ($bfloat z1)) 
            (bfloat-gamma-incomplete ($bfloat a) ($bfloat z2))))

      ((complex-bigfloat-numerical-eval-p a z1 z2)
       (let ((ca  (add ($bfloat ($realpart a)) 
                       (mul '$%i ($bfloat ($imagpart a)))))
             (cz1 (add ($bfloat ($realpart z1))
                       (mul '$%i ($bfloat ($imagpart z1)))))
             (cz2 (add ($bfloat ($realpart z2))
                       (mul '$%i ($bfloat ($imagpart z2))))))
       (sub (complex-bfloat-gamma-incomplete ca cz1)
            (complex-bfloat-gamma-incomplete ca cz2))))

      ;; Check for transformations and argument simplification

      ((and $gamma_expand (mplusp a) (integerp (cadr a)))
       ;; Expand gamma_incomplete_generalized(a+n,z1,z2) with n an integer
       (let ((n (cadr a))
             (a (simplify (cons '(mplus) (cddr a)))))
         (cond
           ((> n 0)
            (mul
              (simplify (list '($pochhammer) a n))
              (add
                (simplify (list '(%gamma_incomplete_generalized) a z1 z2))
                (mul
                  (power '$%e (mul -1 z1))
                  (let ((index (gensumindex)))
                    (dosum
                      (div
                        (power z1 (add a index -1))
                        (simplify (list '($pochhammer) a index)))
                      index 1 n t)))
                (mul -1
                  (power '$%e (mul -1 z2))
                  (let ((index (gensumindex)))
                    (dosum
                      (div
                        (power z2 (add a index -1))
                        (simplify (list '($pochhammer) a index)))
                      index 1 n t))))))

           ((< n 0)
            (setq n (- n))
            (add
              (mul
                (div
                  (power -1 n)
                  ($factor (simplify (list '($pochhammer) (sub 1 a) n))))
                (simplify (list '(%gamma_incomplete_generalized) a z1 z2)))
              (mul -1
                (power '$%e (mul -1 z2))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z1 (add a index (- n) -1))
                      (simplify (list '($pochhammer) (sub a n) index)))
                    index 1 n t)))
              (mul
                (power '$%e (mul -1 z2))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z2 (add a index (- n) -1))
                      (simplify (list '($pochhammer) (sub a n) index)))
                    index 1 n t))))))))

      (t (eqtest (list '(%gamma_incomplete_generalized) a z1 z2) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Regularized Incomplete Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $gamma_incomplete_regularized (a z)
  (simplify (list '(%gamma_incomplete) (resimplify a) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $gamma_incomplete_regularized %gamma_incomplete_regularized alias)
(defprop $gamma_incomplete_regularized %gamma_incomplete_regularized verb)

(defprop %gamma_incomplete_regularized 
         $gamma_incomplete_regularized reversealias)
(defprop %gamma_incomplete_regularized 
         $gamma_incomplete_regularized noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generalized Incomplete Gamma function has not mirror symmetry for z1 or z2 
;;; on the negative real axis. 
;;; We support a conjugate-function which test this case.

(defprop %gamma_incomplete_regularized
         conjugate-gamma-incomplete-regularized conjugate-function)

(defun conjugate-gamma-incomplete-regularized (args)
  (let ((a (first args)) (z (second args)))
    (cond ((off-negative-real-axisp z)
           ;; z definitly not on the negative real axis. Mirror symmetry.
	   (simplify
             (list
              '(%gamma_incomplete_regularized)
               (simplify (list '($conjugate) a))
               (simplify (list '($conjugate) z)))))
	  (t
           ;; On the negative real axis or no information. Unsimplified.
           (list
            '($conjugate simp)
             (simplify (list '(%gamma_incomplete_regularized) a z)))))))

;;; Regularized Incomplete Gamma function is a simplifying function

(defprop %gamma_incomplete_regularized 
         simp-gamma-incomplete-regularized operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Differentiation of Regularized Incomplete Gamma function

(defprop %gamma_incomplete_regularized
  ((a z)
   ;; The derivative wrt a in terms of hypergeometric_generalized 2F2 function
   ;; and the Regularized Generalized Incomplete Gamma function 
   ;; (functions.wolfram.com)
   ((mplus)
      ((mtimes)
         ((%gamma) a)
         ((mexpt) z a)
         (($hypergeometric_generalized)
            ((mlist) a a)
            ((mlist) ((mplus) 1 a) ((mplus) 1 a))
            ((mtimes) -1 z)))
      ((mtimes)
         ((%gamma_incomplete_generalized_regularized) a z 0)
         ((mplus)
            ((%log) z)
            ((mtimes) -1 ((mqapply) (($psi array) 0) a)))))
   ;; The derivative wrt z
   ((mtimes)
      ((mexpt) $%e ((mtimes) -1 z))
      ((mexpt) z ((mplus) -1 a))
      ((mexpt) ((%gamma) a) -1)))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-gamma-incomplete-regularized (expr ignored simpflag)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((a (simpcheck (cadr expr)  simpflag))
        (z (simpcheck (caddr expr) simpflag))
        (ratorder 0))

    (cond

      ;; Check for specific values

      ((zerop1 z)
       (let ((sgn ($sign ($realpart a))))
         (cond ((eq sgn '$neg) (domain-error 0 'gamma_incomplete_regularized))
               ((eq sgn '$zero) (domain-error 0 'gamma_incomplete))
               ((member sgn '($pos $pz)) 1)
               (t (eqtest (list '(%gamma_incomplete) a z) expr)))))  

      ((zerop1 a) 0)
      ((eq z '$inf) 0)

      ;; Check for numerical evaluation in Float or Bigfloat precision

      ((float-numerical-eval-p a z)
       (complexify
       ;; gamma_incomplete returns a regularized result
         (gamma-incomplete ($float a) ($float z) t)))

      ((complex-float-numerical-eval-p a z)
       (let ((ca (complex ($float ($realpart a)) ($float ($imagpart a))))
             (cz (complex ($float ($realpart z)) ($float ($imagpart z)))))
         ;; gamma_incomplete returns a regularized result
         (complexify (gamma-incomplete ca cz t))))
           
      ((bigfloat-numerical-eval-p a z)
       (div (bfloat-gamma-incomplete ($bfloat a) ($bfloat z)) 
            (simplify (list '(%gamma) ($bfloat a)))))

      ((complex-bigfloat-numerical-eval-p a z)
       (let ((ca (add ($bfloat ($realpart a)) 
                      (mul '$%i ($bfloat ($imagpart a)))))
             (cz (add ($bfloat ($realpart z)) 
                 (mul '$%i ($bfloat ($imagpart z))))))
       ($rectform
         (div
           (complex-bfloat-gamma-incomplete ca cz)
           (simplify (list '(%gamma) ca))))))

      ;; Check for transformations and argument simplification

      ((and $gamma_expand (integerp a))
       ;; An integer. Expand the expression.
       (let ((sgn ($sign a)))
         (cond
           ((member sgn '($pos $pz))
            (mul              
              (power '$%e (mul -1 z))
              (let ((index (gensumindex)))
                (dosum
                  (div 
                    (power z index)
                    (list '(%gamma) (add index 1)))
                  index 0 (sub a 1) t))))
           ((member sgn '($neg $nz)) 0)
           (t (eqtest (list '(%gamma_incomplete) a z) expr)))))

      ((and $gamma_expand (setq ratorder (max-numeric-ratio-p a 2)))
       ;; We have a half integral order and $gamma_expand is not NIL.
       ;; We expand in a series with the Erfc function
       (setq ratorder (- ratorder (/ 1 2)))
       (when *debug-gamma*
         (format t "~&SIMP-GAMMA-INCOMPLETE-REGULARIZED in RATORDER~%")
         (format t "~&   : a        = ~A~%" a)
         (format t "~&   : ratorder = ~A~%" ratorder))
       (cond
         ((equal ratorder 0)
          (simplify (list '(%erfc) (power z '((rat) 1 2)))))

         ((> ratorder 0)
          (add                               
            (simplify (list '(%erfc) (power z '((rat) 1 2))))
            (mul
              (power -1 (sub ratorder 1))
              (power '$%e (mul -1 z))
              (power z (div 1 2))
              (div 1 (simplify (list '(%gamma) a)))             
              (let ((index (gensumindex)))
                (dosum
                  (mul
                    (power (mul -1 z) index)
                    (list '($pochhammer) (sub (div 1 2) ratorder)   
                                         (sub ratorder (add index 1))))
                  index 0 (sub ratorder 1) t)))))

         ((< ratorder 0)
          (setq ratorder (- ratorder))
          (add
            (simplify (list '(%erfc) (power z '((rat) 1 2))))
            (mul -1
              (power '$%e (mul -1 z))
              (power z (sub (div 1 2) ratorder))
              (div 1 (simplify (list '(%gamma) (sub (div 1 2) ratorder))))
              (let ((index (gensumindex)))
                (dosum
                  (div
                    (power z index)
                    (list '($pochhammer) (sub (div 1 2) ratorder) 
                                         (add index 1)))
                  index 0 (sub ratorder 1) t)))))))

      ((and $gamma_expand (mplusp a) (integerp (cadr a)))
       (when *debug-gamma* 
         (format t "~&SIMP-GAMMA-INCOMPLETE-REGULARIZED in COND (mplusp)~%"))
       (let ((n (cadr a))
             (a (simplify (cons '(mplus) (cddr a)))))
         (cond
           ((> n 0)
            (add
              (simplify (list '(%gamma_incomplete_regularized) a z))
              ;; We factor the second summand. 
              ;; Some factors vanish and the result is more readable.
              ($factor
                (mul
                  (power '$%e (mul -1 z))
                  (power z (add a -1))
                  (div 1 (simplify (list '(%gamma) a)))
                  (let ((index (gensumindex)))
                    (dosum
                      (div
                        (power z index)
                        (simplify
                          (list '($pochhammer) a index)))
                      index 1 n t))))))
           ((< n 0)
            (setq n (- n))
            (add
              (simplify (list '(%gamma_incomplete_regularized) a z))
              ;; We factor the second summand.
              ($factor
                (mul -1
                  (power '$%e (mul -1 z))
                  (power z (sub a (add n 1)))
                  (div 1 (simplify (list '(%gamma) (add a (- n)))))
                  (let ((index (gensumindex)))
                    (dosum
                      (div
                        (power z index)
                        (list '($pochhammer) (add a (- n)) index))
                      index 1 n t)))))))))
      
      (t (eqtest (list '(%gamma_incomplete_regularized) a z) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Logarithm of the Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $log_gamma (z)
  (simplify (list '(%log_gamma) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $log_gamma %log_gamma alias)
(defprop $log_gamma %log_gamma verb)

(defprop %log_gamma $log_gamma reversealias)
(defprop %log_gamma $log_gamma noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %log_gamma simp-log-gamma operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %log_gamma
  ((z)
   ((mqapply) (($psi array) 0) z))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-log-gamma (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((and (mnump z)
          (or (zerop1 z)
              (and (eq ($sign z) '$neg)
                   (zerop1 (sub z ($truncate z))))))
     ;; We have zero, a negative integer or a float or bigfloat representation.
     (merror "log_gamma(~:M) is undefined." z))

    ((eq z '$inf) '$inf)

    ;; Check for numerical evaluation

    ((float-numerical-eval-p z)
     (complexify (log-gamma-lanczos (complex ($float z) 0))))

    ((complex-float-numerical-eval-p z)
     (complexify 
       (log-gamma-lanczos 
         (complex ($float ($realpart z)) ($float ($imagpart z))))))

    ((bigfloat-numerical-eval-p z) 
     (bfloat-log-gamma ($bfloat z)))

    ((complex-bigfloat-numerical-eval-p z)
     (complex-bfloat-log-gamma 
       (add ($bfloat ($realpart z)) (mul '$%i ($bfloat ($imagpart z))))))

    ;; Transform to Logarithm of Factorial for integer values
    ;; At this point the integer value is positive and not zero.

    ((integerp z)
     (simplify (list '(%log) (simplify (list '(mfactorial) (- z 1))))))

    (t (eqtest (list '(%log_gamma) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The functions log-gamma-lanczos, bfloat-log-gamma and 
;;; complex-bfloat-log-gamma are modified versions of the related functions
;;; gamma-lanczos, bffac and cbffac. The functions return the Logarithm of
;;; the Gamma function. If we have to calculate the quotient of Gamma functions,
;;; e. g. for the Beta function, it is much more appropriate to use the
;;; logarithmic versions to avoid overflow.
;;;
;;; Be careful log(gamma(z)) is only for realpart(z) positive equal to 
;;; log_gamma(z). For a negative realpart(z) log_gamma differ by multiple of 
;;; %pi from log(gamma(z)). But we always have exp(log_gamma(z))= gamma(z).
;;; The terms to get the transformation for log_gamma(-z) are taken from
;;; functions.wolfram.com.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun log-gamma-lanczos (z)
  (declare (type (complex flonum) z)
           (optimize (safety 3)))
  (let ((c (make-array 15 :element-type 'flonum
                       :initial-contents
                       '(0.99999999999999709182
                         57.156235665862923517
                         -59.597960355475491248
                         14.136097974741747174
                         -0.49191381609762019978
                         .33994649984811888699e-4
                         .46523628927048575665e-4
                         -.98374475304879564677e-4
                         .15808870322491248884e-3
                         -.21026444172410488319e-3
                         .21743961811521264320e-3
                         -.16431810653676389022e-3
                         .84418223983852743293e-4
                         -.26190838401581408670e-4
                         .36899182659531622704e-5))))
    (declare (type (simple-array flonum (15)) c))
    (if (minusp (realpart z))
        (let ((z (- z)))
          (-
            (+
              (*
                (- (float pi))
                (complex 0 1)
                (abs (floor (realpart z)))
                (- 1 (abs (signum (imagpart z)))))
              (log (float pi))
              (- (log (- z)))
              (- (log (sin (* (float pi) (- z (floor (realpart z)))))))
              (* 
                (float pi)
                (complex 0 1)
                (floor (realpart z))
                (signum (imagpart z))))
            (log-gamma-lanczos z)))
        (let* ((z (- z 1))
               (zh (+ z 1/2))
               (zgh (+ zh 607/128))
               (lnzp (* (/ zh 2) (log zgh)))
               (ss 
                (do ((sum 0.0)
                     (pp (1- (length c)) (1- pp)))
                    ((< pp 1)
                     sum)
                  (incf sum (/ (aref c pp) (+ z pp))))))
          (+ (log (sqrt (float (* 2 pi))))
             (log (+ ss (aref c 0)))
             (+ (- zgh) (* 2 lnzp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfloat-log-gamma (z)
  (let (($ratprint nil)
        (bigfloat%pi  ($bfloat '$%pi)))
  (cond
    ((eq ($sign z) '$neg)
     (let ((z (mul -1 z)))
       (sub
         (add
           (mul -1 bigfloat%pi '$%i
             (simplify (list '(mabs) (simplify (list '($floor) ($realpart z)))))
             (sub 1
               (simplify 
                 (list '(mabs) (simplify (list '(%signum) ($imagpart z)))))))
           (simplify (list '(%log) bigfloat%pi))
           (mul -1 (simplify (list '(%log) (mul -1 z))))
           (mul -1 
             (simplify (list '(%log) 
               (simplify (list '(%sin) 
                 (mul 
                   bigfloat%pi 
                   (sub z (simplify (list '($floor) ($realpart z))))))))))
           (mul
             bigfloat%pi '$%i
             (simplify (list '($floor) ($realpart z)))
             (simplify (list '(%signum) ($imagpart z)))))
         (bfloat-log-gamma z))))
    (t
     (let* ((k (* 2 (+ 1 ($entier (* 0.41 $fpprec)))))
            (m ($bfloat bigfloatone))
            (z+k (add z k -1))
            (y (power z+k 2))
            (x ($bfloat bigfloatzero))
            (ii))
       (dotimes (i (/ k 2))
         (setq ii (* 2 (+ i 1)))
         (setq m (mul m (add z ii -2) (add z ii -1)))
         (setq x (div
                   (add x
                        (div ($bern (+ k (- ii) 2))
                             (* (+ k (- ii) 1) (+ k (- ii) 2))))
                   y)))
       (add
         (div (simplify (list '(%log) (mul 2 bigfloat%pi z+k))) 2)
         (mul z+k (add (simplify (list '(%log) z+k)) x -1))
         (mul -1 (simplify (list '(%log) m)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun complex-bfloat-log-gamma (z)
  (let (($ratprint nil)
        (bigfloat%pi ($bfloat '$%pi)))
  (cond
    ((eq ($sign ($realpart z)) '$neg)
     (let ((z (mul -1 z)))
       (sub
         (add
           (mul -1 bigfloat%pi '$%i
             (simplify (list '(mabs) (simplify (list '($floor) ($realpart z)))))
             (sub 1
               (simplify 
                 (list '(mabs) (simplify (list '(%signum) ($imagpart z)))))))
           (simplify (list '(%log) bigfloat%pi))
           (mul -1 (simplify (list '(%log) (mul -1 z))))
           (mul -1 
             (simplify (list '(%log) 
               (simplify (list '(%sin) 
                 (mul 
                   bigfloat%pi 
                   (sub z (simplify (list '($floor) ($realpart z))))))))))
           (mul
             bigfloat%pi '$%i
             (simplify (list '($floor) ($realpart z)))
             (simplify (list '(%signum) ($imagpart z)))))
         (bfloat-log-gamma z))))
    (t
     (let* ((k (* 2 (+ 1 ($entier (* 0.41 $fpprec)))))
            (m ($bfloat bigfloatone))
            (z+k (add z k -1))
            (y ($rectform (power z+k 2)))
            (x ($bfloat bigfloatzero))
            (ii))
       (dotimes (i (/ k 2))
         (setq ii (* 2 (+ i 1)))
         (setq m ($rectform (mul m (add z ii -2) (add z ii -1))))
         (setq x ($rectform
                   (div
                     (add x 
                       (div ($bern (+ k (- ii) 2))
                            (* (+ k (- ii) 1) (+ k (- ii) 2))))
                   y))))
       ($rectform
         (add
           (div (simplify (list '(%log) (mul 2 bigfloat%pi z+k))) 2)
           (mul z+k (add (simplify (list '(%log) z+k)) x -1))
           (mul -1 (simplify (list '(%log) m))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Error function Erf(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $erf (z)
  (simplify (list '(%erf) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $erf %erf alias)
(defprop $erf %erf verb)

(defprop %erf $erf reversealias)
(defprop %erf $erf noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; erf has mirror symmetry

(defprop %erf t commutes-with-conjugate)

;;; erf is an odd function

(defprop %erf odd-function-reflect reflection-rule)

;;; erf is a simplifying function

(defprop %erf simp-erf operators)

;;; Derivative of the Error function erf

(defprop %erf 
  ((z)
   ((mtimes) 2 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) $%e ((mtimes) -1 ((mexpt) z 2)))))
  grad)

;;; Integral of the Error function erf

(defprop %erf
  ((z)
   ((mplus)
     ((mtimes) 
        ((mexpt) $%pi ((rat) -1 2))
        ((mexpt) $%e ((mtimes) -1 ((mexpt) z 2))))
     ((mtimes) z ((%erf) z))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-erf (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((zerop1 z) z)
    ((eq z '$inf) 1)
    ((eq z '$minf) -1)

    ;; Check for numerical evaluation

    ((float-numerical-eval-p z)
     (erf ($float z)))
    ((complex-float-numerical-eval-p z)
     (complexify 
       (complex-erf (complex ($float ($realpart z)) ($float ($imagpart z))))))
    ((bigfloat-numerical-eval-p z)
     (bfloat-erf ($bfloat z)))
    ((complex-bigfloat-numerical-eval-p z)
     (complex-bfloat-erf 
       (add ($bfloat ($realpart z)) (mul '$%i ($bfloat ($imagpart z))))))

    ;; Argument simplification
    
    ((taylorize (mop expr) (second expr)))
    ((and $erf_%iargs 
          (not $erf_representation)
          (multiplep z '$%i))
     (mul '$%i (simplify (list '(%erfi) (coeff z '$%i 1)))))
    ((apply-reflection-simp (mop expr) z $trigsign))
    
    ;; Representation through equivalent functions
    
    ($hypergeometric_representation
      (mul 2 z 
           (power '$%pi (div 1 2))
           (list '(%hypergeometric)
                 (list '(mlist) (div 1 2))
                 (list '(mlist) (div 3 2))
                 (mul -1 (power z 2)))))

    (t
     (eqtest (list '(%erf) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erf (z)
  ;; We use the slatec routine for float values.
  (slatec:derf (float z)))

;;; This would be the code when using gamma-incomplete.
;  (realpart
;    (*
;      (signum z)
;      (- 1.0 
;        (* (/ (sqrt (float pi))) (gamma-incomplete 0.5 (expt z 2.0)))))))

(defun complex-erf (z)
  (let ((result
          (*
            (/ (sqrt (expt z 2)) z)
            (- 1.0 
              (* (/ (sqrt (float pi))) (gamma-incomplete 0.5 (expt z 2.0)))))))
    (cond
      ((= (imagpart z) 0.0)
       ;; Pure real argument, the result is real
       (complex (realpart result) 0.0))
      ((= (realpart z) 0.0)
       ;; Pure imaginary argument, the result is pure imaginary
       (complex 0.0 (imagpart result)))
      (t
        result))))

(defun bfloat-erf (z)
  ;; The argument is real, the result is real too
  ($realpart
    (mul
      (simplify (list '(%signum) z))
      (sub 1.0
        (mul 
          (div 1.0 (power ($bfloat '$%pi) 0.5))
          (bfloat-gamma-incomplete ($bfloat 0.5) ($bfloat (power z 2))))))))

(defun complex-bfloat-erf (z)
  (let* (($ratprint nil)
         (result 
           ($rectform
             (mul
               ($rectform (div (power (power z 2) 0.5) z))
               (sub 1.0
                 (mul 
                   (div 1.0 (power ($bfloat '$%pi) 0.5))
                   (complex-bfloat-gamma-incomplete 
                     ($bfloat 0.5) 
                     ($bfloat (power z 2)))))))))
    (cond
      ((zerop1 ($imagpart z))
       ;; Pure real argument, the result is real
       ($realpart result))
      ((zerop1 ($realpart z))
        ;; Pure imaginary argument, the result is pure imaginary
        (mul '$%i ($imagpart result)))
      (t
       ;; A general complex result
       result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Generalized Error function Erf(z1,z2)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $erf_generalized (z1 z2)
  (simplify (list '(%erf_generalized) (resimplify z1) (resimplify z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $erf_generalized %erf_generalized alias)
(defprop $erf_generalized %erf_generalized verb)

(defprop %erf_generalized $erf_generalized reversealias)
(defprop %erf_generalized $erf_generalized noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generalized Erf has mirror symmetry

(defprop %erf_generalized t commutes-with-conjugate)

;;; Generalized Erf is antisymmetric Erf(z1,z2) = - Erf(z2,z1)

(eval-when
    #+gcl (load eval)
    #-gcl (:load-toplevel :execute)
    (let (($context '$global) (context '$global))
      (meval '(($declare) %erf_generalized $antisymmetric))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %erf_generalized simp-erf-generalized operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %erf_generalized 
  ((z1 z2)
   ;; derivative wrt z1
   ((mtimes) -2 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) $%e ((mtimes) -1 ((mexpt) z1 2))))
   ;; derviative wrt z2
   ((mtimes) 2 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) $%e ((mtimes) -1 ((mexpt) z2 2)))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-erf-generalized (expr ignored simpflag)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((z1 (simpcheck (cadr expr) simpflag))
        (z2 (simpcheck (caddr expr) simpflag)))
    (cond

      ;; Check for specific values

      ((and (zerop1 z1) (zerop1 z2)) 0)
      ((zerop1 z1) (simplify (list '(%erf) z2)))
      ((zerop1 z2) (mul -1 (simplify (list '(%erf) z1))))
      ((eq z2 '$inf) (sub 1 (simplify (list '(%erf) z1))))
      ((eq z2 '$minf) (sub (mul -1 (simplify (list '(%erf) z1))) 1))
      ((eq z1 '$inf) (sub (simplify (list '(%erf) z2)) 1))
      ((eq z1 '$minf) (add (simplify (list '(%erf) z2)) 1))

      ;; Check for numerical evaluation. Use erf(z1,z2) = erf(z2)-erf(z1)

      ((float-numerical-eval-p z1 z2)
       (- (erf ($float z2)) (erf ($float z1))))
      ((complex-float-numerical-eval-p z1 z2)
       (complexify 
         (- 
           (complex-erf 
             (complex ($float ($realpart z2)) ($float ($imagpart z2))))
           (complex-erf 
             (complex ($float ($realpart z1)) ($float ($imagpart z1)))))))
      ((bigfloat-numerical-eval-p z1 z2)
       (sub
         (bfloat-erf ($bfloat z2))
         (bfloat-erf ($bfloat z1))))
      ((complex-bigfloat-numerical-eval-p z1 z2)
       (sub
         (complex-bfloat-erf 
           (add ($bfloat ($realpart z2)) (mul '$%i ($bfloat ($imagpart z2)))))
         (complex-bfloat-erf 
           (add ($bfloat ($realpart z1)) (mul '$%i ($bfloat ($imagpart z1)))))))

      ;; Argument simplification
      
      ((and $trigsign (great (mul -1 z1) z1) (great (mul -1 z2) z2))
       (mul -1 (simplify (list '(%erf_generalized) (mul -1 z1) (mul -1 z2)))))

      ;; Transformation to Erf

      ($erf_representation
       (sub (simplify (list '(%erf) z2)) (simplify (list '(%erf) z1))))

      (t
       (eqtest (list '(%erf_generalized) z1 z2) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Complementary Error function Erfc(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $erfc (z)
  (simplify (list '(%erfc) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $erfc %erfc alias)
(defprop $erfc %erfc verb)

(defprop %erfc $erfc reversealias)
(defprop %erfc $erfc noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %erfc t commutes-with-conjugate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %erfc simp-erfc operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %erfc 
  ((z)
   ((mtimes) -2 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) $%e ((mtimes) -1 ((mexpt) z 2)))))
  grad)

;;; Integral of the Error function erfc

(defprop %erfc
  ((z)
   ((mplus)
     ((mtimes) -1
        ((mexpt) $%pi ((rat) -1 2))
        ((mexpt) $%e ((mtimes) -1 ((mexpt) z 2))))
     ((mtimes) z ((%erfc) z))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-erfc (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((zerop1 z) 1)
    ((eq z '$inf) 0)
    ((eq z '$minf) 2)

    ;; Check for numerical evaluation. Use erfc(z) = 1-erf(z).

    ((float-numerical-eval-p z)
     (- 1.0 (erf ($float z))))
    ((complex-float-numerical-eval-p z)
     (complexify 
       (- 1.0 
         (complex-erf 
           (complex ($float ($realpart z)) ($float ($imagpart z)))))))
    ((bigfloat-numerical-eval-p z)
     (sub 1.0 (bfloat-erf ($bfloat z))))
    ((complex-bigfloat-numerical-eval-p z)
     (sub 1.0
       (complex-bfloat-erf 
         (add ($bfloat ($realpart z)) (mul '$%i ($bfloat ($imagpart z)))))))

    ;; Argument simplification

    ((taylorize (mop expr) (second expr)))
    ((and $trigsign (great (mul -1 z) z))
     (sub 2 (simplify (list  '(%erfc) (mul -1 z)))))

    ;; Transformation to Erf

    ($erf_representation
     (sub 1 (simplify (list '(%erf) z))))
    
    ($hypergeometric_representation
      (sub 1
        (mul 2 z 
             (power '$%pi (div 1 2))
             (list '(%hypergeometric)
                   (list '(mlist) (div 1 2))
                   (list '(mlist) (div 3 2 ))
                   (mul -1 (power z 2))))))

    (t
     (eqtest (list '(%erfc) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Imaginary Error function Erfi(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $erfi (z)
  (simplify (list '(%erfi) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $erfi %erfi alias)
(defprop $erfi %erfi verb)

(defprop %erfi $erfi reversealias)
(defprop %erfi $erfi noun)

;;; erfi has mirror symmetry

(defprop %erfi t commutes-with-conjugate)

;;; erfi is an odd function

(defprop %erfi odd-function-reflect reflection-rule)

;;; erfi is an simplifying function

(defprop %erfi simp-erfi operators)

;;; Derivative of the Error function erfi

(defprop %erfi
  ((z)
   ((mtimes) 2 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) $%e ((mexpt) z 2))))
  grad)

;;; Integral of the Error function erfi

(defprop %erfi
  ((z)
   ((mplus)
     ((mtimes) -1
        ((mexpt) $%pi ((rat) -1 2))
        ((mexpt) $%e ((mexpt) z 2)))
     ((mtimes) z ((%erfi) z))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-erfi (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((zerop1 z) z)
    ((eq z '$inf) '$inf)
    ((eq z '$minf) '$minf)

    ;; Check for numerical evaluation. Use erfi(z) = -%i*erf(%i*z).

    ((float-numerical-eval-p z)
     ;; For a real argument z the value of erfi is real.
     (realpart (* (complex 0 -1) (complex-erf (complex 0 ($float z))))))
    ((complex-float-numerical-eval-p z)
     (complexify 
       (* 
         (complex 0 -1)
         (complex-erf 
           (complex (- ($float ($imagpart z))) ($float ($realpart z)))))))
    ((bigfloat-numerical-eval-p z)
     ;; For a real argument z the value of erfi is real.
     ($realpart
       (mul -1
         '$%i
         (complex-bfloat-erf ($bfloat (mul '$%i z))))))
    ((complex-bigfloat-numerical-eval-p z)
     ($rectform
       (mul -1
         '$%i
         (complex-bfloat-erf 
           (add ($bfloat (mul -1 ($imagpart z)))
                (mul '$%i ($bfloat ($realpart z))))))))

    ;; Argument simplification

    ((taylorize (mop expr) (second expr)))
    ((and $erf_%iargs
          (multiplep z '$%i))
     (mul '$%i (simplify (list '(%erf) (coeff z '$%i 1)))))
    ((apply-reflection-simp (mop expr) z $trigsign))

    ;; Representation through equivalent functions

    ($erf_representation
     (mul -1 '$%i (simplify (list '(%erf) (mul '$%i z)))))
    
    ($hypergeometric_representation
      (mul 2 z
        (power '$%pi (div 1 2))
               (list '(%hypergeometric)
                     (list '(mlist) (div 1 2))
                     (list '(mlist) (div 3 2))
                     (power z 2))))

    (t
     (eqtest (list '(%erfi) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Fresnel Integral S(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $fresnel_s (z)
  (simplify (list '(%fresnel_s) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $fresnel_s %fresnel_s alias)
(defprop $fresnel_s %fresnel_s verb)

(defprop %fresnel_s $fresnel_s reversealias)
(defprop %fresnel_s $fresnel_s noun)

;;; fresnel_s is a simplifying function

(defprop %fresnel_s simp-fresnel-s operators)

;;; fresnel_s has mirror symmetry

(defprop %fresnel_s t commutes-with-conjugate)

;;; fresnel_s is an odd function
;;;
;;; Maxima has two mechanism to define a reflection property
;;; 1. Declare the feature oddfun or evenfun
;;; 2. Put a reflection rule on the property list
;;;
;;; The second way is used for the trig functions. We use it here too.

;;; This would be the first way to give the property of an odd function.
;(eval-when
;    #+gcl (load eval)
;    #-gcl (:load-toplevel :execute)
;    (let (($context '$global) (context '$global))
;      (meval '(($declare) %fresnel_s $oddfun))))

(defprop %fresnel_s odd-function-reflect reflection-rule)

;;; Differentiation of the Fresnel Integral S

(defprop %fresnel_s
  ((z)
   ((%sin) ((mtimes) ((rat) 1 2) $%pi ((mexpt) z 2))))
  grad)

;;; Integration of the Fresnel Integral S

(defprop %fresnel_s
  ((z)
   ((mplus) 
      ((mtimes) z ((%fresnel_s) z))
      ((mtimes) 
         ((mexpt) $%pi -1)
         ((%cos) ((mtimes) ((rat) 1 2) $%pi ((mexpt) z 2))))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-fresnel-s (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((zerop1 z) z)
    ((eq z '$inf)  '((rat) 1 2))
    ((eq z '$minf) '((rat) -1 2))
    
    ;; Check for numerical evaluation
    
     ((float-numerical-eval-p z)
      (nth-value 0 (fresnel ($float z))))
    
     ((complex-float-numerical-eval-p z)
      (cond
        ((zerop1 ($realpart z))
         ;; A pure imaginary argument. Use fresnel_s(%i*x)=-%i*fresnel_s(x).
         (mul -1 '$%i
           (nth-value 0 (fresnel ($float ($imagpart z))))))
        (t
         (complexify 
           (nth-value 0 
             (complex-fresnel (complex ($float ($realpart z)) 
                                       ($float ($imagpart z)))))))))

    ((bigfloat-numerical-eval-p z)
     (nth-value 0 (bfloat-fresnel ($bfloat z))))

    ((complex-bigfloat-numerical-eval-p z)
     (cond
       ((zerop1 ($realpart z))
        ;; A pure imaginary argument. Use fresnel_s(%i*x)=-%i*fresnel_s(x).
        (nth-value 0
          (mul -1 '$%i 
            (bfloat-fresnel ($bfloat ($imagpart z))))))
       (t
        (nth-value 0 
          (complex-bfloat-fresnel 
            (add ($bfloat ($realpart z)) 
                 (mul '$%i ($bfloat ($imagpart z)))))))))

    ;; Check for argument simplification

    ((taylorize (mop expr) (second expr)))
    ((and $%iargs (multiplep z '$%i))
     (mul -1 '$%i (simplify (list '(%fresnel_s) (coeff z '$%i 1)))))
    ((apply-reflection-simp (mop expr) z $trigsign))

    ;; Representation through equivalent functions

    ($erf_representation
      (mul
        (div (add 1 '$%i) 4)
        (add
          (simplify 
            (list 
              '(%erf) 
              (mul (div (add 1 '$%i) 2) (power '$%pi (div 1 2)) z)))
          (mul -1 '$%i
            (simplify 
              (list 
                '(%erf) 
                (mul (div (sub 1 '$%i) 2) (power '$%pi (div 1 2)) z)))))))

    ($hypergeometric_representation
      (mul 
        (div (mul '$%pi (power z 3)) 6)
        (list '(%hypergeometric) 
          (list '(mlist) (div 3 4))
          (list '(mlist) (div 3 2) (div 7 4))
          (mul -1 (div (mul (power '$%pi 2) (power z 4)) 16)))))

    (t
     (eqtest (list '(%fresnel_s) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Fresnel Integral C(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $fresnel_c (z)
  (simplify (list '(%fresnel_c) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $fresnel_c %fresnel_c alias)
(defprop $fresnel_c %fresnel_c verb)

(defprop %fresnel_c $fresnel_c reversealias)
(defprop %fresnel_c $fresnel_c noun)

;;; fresnel_c is a simplifying function

(defprop %fresnel_c simp-fresnel-c operators)

;;; fresnel_c has mirror symmetry

(defprop %fresnel_c t commutes-with-conjugate)

;;; fresnel_c is an odd function
;;; Maxima has two mechanism to define a reflection property
;;; 1. Declare the feature oddfun or evenfun
;;; 2. Put a reflection rule on the property list
;;;
;;; The second way is used for the trig functions. We use it here too.

(defprop %fresnel_c odd-function-reflect reflection-rule)

;;; Differentiation of the Fresnel Integral C

(defprop %fresnel_c
  ((z)
   ((%cos) ((mtimes) ((rat) 1 2) $%pi ((mexpt) z 2))))
  grad)

;;; Integration of the Fresnel Integral C

(defprop %fresnel_c
  ((z)
   ((mplus) 
      ((mtimes) z ((%fresnel_c) z))
      ((mtimes) -1 
         ((mexpt) $%pi -1)
         ((%sin) ((mtimes) ((rat) 1 2) $%pi ((mexpt) z 2))))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-fresnel-c (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((zerop1 z) z)
    ((eq z '$inf)  '((rat) 1 2))
    ((eq z '$minf) '((rat) -1 2))
    
    ;; Check for numerical evaluation
    
     ((float-numerical-eval-p z)
      (nth-value 1 (fresnel ($float z))))

     ((complex-float-numerical-eval-p z)
      (cond
        ((zerop1 ($realpart z))
         ;; A pure imaginary argument. Use fresnel_c(%i*x)=%i*fresnel_c(x).
         (mul '$%i
           (nth-value 1 (fresnel ($float ($imagpart z))))))
        (t
         (complexify 
           (nth-value 1
             (complex-fresnel (complex ($float ($realpart z)) 
                                       ($float ($imagpart z)))))))))

    ((bigfloat-numerical-eval-p z)
     (nth-value 1 (bfloat-fresnel ($bfloat z))))

    ((complex-bigfloat-numerical-eval-p z)
     (cond
       ((zerop1 ($realpart z))
        ;; A pure imaginary argument. Use fresnel_c(%i*x)=%i*fresnel_c(x).
        (nth-value 1
          (mul -1 '$%i 
            (bfloat-fresnel ($bfloat ($imagpart z))))))
       (t
        (nth-value 1 
          (complex-bfloat-fresnel 
            (add ($bfloat ($realpart z)) 
                 (mul '$%i ($bfloat ($imagpart z)))))))))

    ;; Check for argument simplification

    ((taylorize (mop expr) (second expr)))
    ((and $%iargs (multiplep z '$%i))
     (mul '$%i (simplify (list '(%fresnel_c) (coeff z '$%i 1)))))
    ((apply-reflection-simp (mop expr) z $trigsign))

    ;; Representation through equivalent functions

    ($erf_representation
      (mul
        (div (sub 1 '$%i) 4)
        (add
          (simplify 
            (list 
              '(%erf) 
              (mul (div (add 1 '$%i) 2) (power '$%pi (div 1 2)) z)))
          (mul '$%i
            (simplify 
              (list 
                '(%erf) 
                (mul (div (sub 1 '$%i) 2) (power '$%pi (div 1 2)) z)))))))

    ($hypergeometric_representation
      (mul z
        (list '(%hypergeometric) 
          (list '(mlist) (div 1 4))
          (list '(mlist) (div 1 2) (div 5 4))
          (mul -1 (div (mul (power '$%pi 2) (power z 4)) 16)))))

    (t
      (eqtest (list '(%fresnel_c) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *fresnel-maxit* 1000)
(defvar *fresnel-eps*   1e-16)
(defvar *fresnel-min*   1e-32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fresnel (x)
  (let ((fpmin *fresnel-min*)
        (eps *fresnel-eps*)
        (maxit *fresnel-maxit*)
        (xmin 1.5)
        (ax (abs x))
        (s 0.0)
        (c (abs x)))
    (when (> ax (sqrt fpmin))
      (cond
        ((< ax xmin)
         (when *debug-gamma*
           (format t "~& in FRESNEL series expansion.~%"))
         (let ((sums 0.0) (sumc ax))
           (do ((sum 0.0) 
                (sign 1) 
                (fact (* (/ (float pi) 2.0) ax ax))
                (term ax)
                (odd t (not odd))
                (test 0.0)
                (n 3 (+ n 2))
                (k 1 (+ k 1)))
               ((> k maxit) (merror "Series expansion failed in fresnel."))
             (setq term (* term (/ fact k)))
             (setq sum (+ sum (/ (* sign term) n)))
             (setq test (* (abs sum) eps))
             (if odd
               (progn
                 (setq sign (- sign))
                 (setq sums sum)
                 (setq sum sumc))
               (progn
                 (setq sumc sum)
                 (setq sum sums)))
             (if (< term test) (return)))
         (setq s sums)
         (setq c sumc)))
        (t
         (let* ((pix2 (* (float pi) ax ax))
                (b (complex 1.0 (- pix2)))
                (d (/ 1.0 b))
                (h d)
                (cs (complex 0.0 0.0)))
           (do ((cc (complex (/ 1.0 fpmin) 0.0))
                (a 0.0)
                (del 0.0)
                (n 1 (+ n 2))
                (k 2 (+ k 1)))
               ((> k maxit)
                (merror "Continued fractions fails in `fresnel'."))
             (setq a (* (- n) (+ n 1)))
             (setq b (+ b (complex 4.0 0.0)))
             (setq d (/ 1.0 (+ (* a d) b)))
             (setq cc (+ b (/ (complex a 0.0) cc)))
             (setq del (* cc d))
             (setq h (* h del))
             (if (< (abs (- del 1.0)) eps) (return)))
           (setq h (* (complex ax (- ax)) h))
           (setq cs (* (complex 0.5 0.5)
                       (- 1.0 (* (complex (cos (* 0.5 pix2))
                                          (sin (* 0.5 pix2)))
                                 h))))
           (setq c (realpart cs))
           (setq s (imagpart cs))))))
    (when (< x 0.0)
        (setq c (- c))
        (setq s (- s)))
    (values s c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun complex-fresnel (z)
  (let ((fpmin *fresnel-min*)
        (eps *fresnel-eps*)
        (maxit *fresnel-maxit*)
        (xmin 1.5)
        (s (complex 0.0 0.0))
        (c z))
    (when (> (abs z) (sqrt fpmin))
      (cond
        ((< (abs z) xmin)
         (when *debug-gamma*
           (format t "~& in FRESNEL series expansion.~%"))
         (let ((sums (complex 0.0 0.0)) (sumc z))
           (do ((sum (complex 0.0 0.0))
                (sign 1)
                (fact (* (/ (float pi) 2.0) z z))
                (term z)
                (odd t (not odd))
                (test 0.0)
                (n 3 (+ n 2))
                (k 1 (+ k 1)))
               ((> k maxit) (merror "Series expansion failed in `fresnel'."))
             (setq term (* term (/ fact k)))
             (setq sum (+ sum (/ (* sign term) n)))
             (setq test (* (abs sum) eps))
             (if odd
               (progn
                 (setq sign (- sign))
                 (setq sums sum)
                 (setq sum sumc))
               (progn
                 (setq sumc sum)
                 (setq sum sums)))
             (if (< (abs term) test) (return)))
           (setq s sums)
           (setq c sumc)))
        (t
         (let* ((z+ (* (complex 0.5 0.5) (sqrt (float pi)) z))
                (z- (* (complex 0.5 -0.5) (sqrt (float pi)) z))
                (erf+ (complex-erf z+))
                (erf- (complex-erf z-)))
           (setq s (* (complex 0.25 0.25) 
                      (+ erf+ (* (complex 0 -1) erf-))))
           (setq c (* (complex 0.25 -0.25)
                      (+ erf+ (* (complex 0 1) erf-))))))))
    (values s c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfloat-fresnel (x)
  (let* ((eps (power ($bfloat 10.0) (- $fpprec)))
         (fpmin (mul eps eps))
         (maxit *fresnel-maxit*)
         (xmin 1.5)
         (ax (simplify (list '(mabs) ($bfloat x))))
         (bigfloatzero ($bfloat bigfloatzero))
         (s bigfloatzero)
         (c ax))
    (when (eq ($sign (sub ax (power fpmin (div 1 2)))) '$pos)
      (cond
        ((eq ($sign (sub ax xmin)) '$neg)
         (when *debug-gamma*
           (format t "~& in FRESNEL series expansion for bfloat.~%"))
         (let ((sums bigfloatzero) (sumc ax))
           (do ((sum bigfloatzero)
                (sign 1)
                (fact (mul (div ($bfloat '$%pi) ($bfloat 2)) ax ax))
                (term ax)
                (odd t (not odd))
                (test bigfloatzero)
                (n 3 (+ n 2))
                (k 1 (+ k 1)))
               ((> k maxit) (merror "Series expansion failed in `fresnel'."))
             (when *debug-gamma*
               (format t "~& in DO-LOOP~%")
               (format t "~&   : sums = ~A~%" sums)
               (format t "~&   : sumc = ~A~%" sumc))
             (setq term (mul term (div fact k)))
             (setq sum (add sum (div (mul sign term) n)))
             (setq test (mul (simplify (list '(mabs) sum)) eps))
             (if odd
               (progn
                 (setq sign (- sign))
                 (setq sums sum)
                 (setq sum sumc))
               (progn
                 (setq sumc sum)
                 (setq sum sums)))
             (if (eq ($sign (sub term test)) '$neg) (return)))
         (setq s sums)
         (setq c sumc)))
        (t
         (let* ((pix2 (mul ($bfloat '$%pi) ax ax))
                (b (add 1.0 (mul -1 '$%i pix2)))
                (d (div 1.0 b))
                (h d)
                (cs bigfloatzero))
           (do ((cc (div 1.0 fpmin))
                (a 0)
                (del bigfloatzero)
                (n 1 (+ n 2))
                (k 2 (+ k 1)))
               ((> k maxit)
                (merror "Continued fractions fails in `fresnel'."))
             (setq a (* (- n) (+ n 1)))
             (setq b (add b 4.0))
             (setq d (cdiv 1.0 (add (mul a d) b)))
             (setq cc (add b (cdiv a cc)))
             (setq del (cmul cc d))
             (setq h (cmul h del))
             (if (eq ($sign (sub (simplify (list '(mabs) (sub del 1.0))) eps)) 
                     '$neg)
               (return)))
           (setq h (cmul (add ax (mul -1 '$%i ax)) h))
           (setq cs (cmul (add 0.5 (mul '$%i 0.5))
                       (sub 1.0 (mul (add ($cos (mul 0.5 pix2))
                                          (mul '$%i ($sin (mul 0.5 pix2))))
                                 h))))
           (setq c ($realpart cs))
           (setq s ($imagpart cs))))))
    (when (eq ($sign x) '$neg)
        (setq c (mul -1 c))
        (setq s (mul -1 s)))
    (values s c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun complex-bfloat-fresnel (z)
  (let* ((eps (power ($bfloat 10.0) (- $fpprec)))
         (fpmin (mul eps eps))
         (maxit *fresnel-maxit*)
         (xmin 1.5)
         (bigfloatzero ($bfloat bigfloatzero))
         (s bigfloatzero)
         (c z))
    (when (eq ($sign (sub (simplify (list '(mabs) z))
                          (power fpmin (div 1 2)))) '$pos)
      (cond
        ((eq ($sign (sub (simplify (list '(mabs) z)) xmin)) '$neg)
         (when *debug-gamma*
           (format t "~& in FRESNEL series expansion.~%"))
         (let ((sums bigfloatzero) (sumc z))
           (do ((sum bigfloatzero)
                (sign 1)
                (fact (cmul (div ($bfloat '$%pi) 2.0) (cmul z z)))
                (term z)
                (odd t (not odd))
                (test bigfloatzero)
                (n 3 (+ n 2))
                (k 1 (+ k 1)))
               ((> k maxit) (merror "Series expansion failed in `fresnel'."))
             (setq term (cmul term (div fact k)))
             (setq sum (add sum (cdiv (mul sign term) n)))
             (setq test (mul (simplify (list '(mabs) sum)) eps))
             (if odd
               (progn
                 (setq sign (- sign))
                 (setq sums sum)
                 (setq sum sumc))
               (progn
                 (setq sumc sum)
                 (setq sum sums)))
             (if (eq ($sign (sub (simplify (list '(mabs) term)) test)) '$neg)
               (return)))
           (setq s sums)
           (setq c sumc)))
        (t
         (let* ((z+ (cmul (add 0.5 (mul '$%i 0.5)) 
                          (cmul (power ($bfloat '$%pi) ($bfloat (div 1 2))) z)))
                (z- (cmul (add 0.5 (mul -1 '$%i 0.5)) 
                          (cmul (power ($bfloat '$%pi) ($bfloat (div 1 2))) z)))
                (erf+ (complex-bfloat-erf z+))
                (erf- (complex-bfloat-erf z-)))
           (setq s (cmul (add 0.25 (mul '$%i 0.25)) 
                         (add erf+ (mul -1 '$%i erf-))))
           (setq c (cmul (add 0.25 (mul -1 '$%i 0.25))
                         (add erf+ (mul '$%i erf-))))))))
    (values s c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Incomplete Beta function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *beta-incomplete-maxit* 10000)
(defvar *beta-incomplete-eps* 1.0e-15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $beta_incomplete (a b z)
  (simplify (list '(%beta_incomplete) a b z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $beta_incomplete %beta_incomplete alias)
(defprop $beta_incomplete %beta_incomplete verb)

(defprop %beta_incomplete $beta_incomplete reversealias)
(defprop %beta_incomplete $beta_incomplete noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %beta_incomplete simp-beta-incomplete operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %beta_incomplete
  ((a b z)
   ;; Derivative wrt a
   ((mplus) 
      ((%beta_incomplete) a b z)
      ((mtimes) -1 
         ((mexpt) ((%gamma) a) 2)
         (($hypergeometric_generalized)
           ((mlist) a a ((mplus) 1 ((mtimes) -1 b)))
           ((mlist) ((mplus) 1 a) ((mplus) 1 a)) 
           z)
         ((mexpt) z a)))
   ;; Derivative wrt b
   ((mplus)
      ((mtimes) 
         (($beta) a b)
         ((mplus) 
            ((mqapply) 
               (($psi array 0) b)
               ((mtimes) -1 ((mqapply) (($psi array) 0) ((mplus) a b)))))
         ((mtimes) -1
            ((%beta_incomplete) b a ((mplus) 1 ((mtimes) -1 z)))
            ((%log) ((mplus) 1 ((mtimes) -1 z))))
         ((mtimes) 
            ((mexpt) ((%gamma) b) 2)
            (($hypergeometric_generalized)
               ((mlist) b b ((mplus) 1 ((mtimes) -1 a)))
               ((mlist) ((mplus) 1 b) ((mplus) 1 b))
               ((mplus) 1 ((mtimes) -1 z)))
            ((mexpt) ((mplus) 1 ((mtimes) -1 z)) b))))
   ;; The derivative wrt z
   ((mtimes)
      ((mexpt) ((mplus) 1 ((mtimes) -1 z)) ((mplus) -1 b)
      ((mexpt) z ((mplus) -1 a)))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-beta-incomplete (expr ignored simpflag)
  (declare (ignore ignored))
  (if (not (= (length expr) 4)) (wna-err '$beta_incomplete))
  (let ((a (simpcheck (cadr expr)   simpflag))
        (b (simpcheck (caddr expr)  simpflag))
        (z (simpcheck (cadddr expr) simpflag)))
    (when *debug-gamma* 
         (format t "~&SIMP-BETA-INCOMPLETE:~%")
         (format t "~&   : a = ~A~%" a)
         (format t "~&   : b = ~A~%" b)
         (format t "~&   : z = ~A~%" z))
    (cond

      ;; Check for specific values

      ((zerop1 z)
       (let ((sgn ($sign ($realpart a))))
         (cond ((eq sgn '$neg) 
                (domain-error 0 'gamma_incomplete))
               ((eq sgn '$zero) 
                (domain-error 0 'gamma_incomplete))
               ((member sgn '($pos $pz)) 
                0)
               (t 
                (eqtest (list '(%beta_incomplete) a b z) expr)))))

      ((and (onep1 z) (or (not (mnump a)) (not (mnump b))))
       (let ((sgn ($sign ($realpart b))))
         (cond ((member sgn '($pos $pz)) 
                (simplify (list '($beta) a b)))
               (t 
                (eqtest (list '(%beta_incomplete) a b z) expr)))))

      ;; Check for numerical evaluation in Float or Bigfloat precision

      ((complex-float-numerical-eval-p a b z)
       (let ((*beta-incomplete-eps* (bigfloat:epsilon ($float 1.0))))
         (to (beta-incomplete (bigfloat:to a) 
                              (bigfloat:to b) 
                              (bigfloat:to z)))))
           
      ((complex-bigfloat-numerical-eval-p a b z)
       (let ((*beta-incomplete-eps* 
               (bigfloat:epsilon (bigfloat:bigfloat 1.0))))
         (to (beta-incomplete (bigfloat:to a) 
                              (bigfloat:to b) 
                              (bigfloat:to z)))))

      (t 
       (eqtest (list '(%beta_incomplete) a b z) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun beta-incomplete (a b z)
  (when *debug-gamma*
    (format t "~&BETA-INCOMPLETE with a=~A,b=~A and z=~A~%" a b z))
  (cond
    ((bigfloat:> (bigfloat:realpart z)
                 (bigfloat:realpart (bigfloat:/ (bigfloat:+ a 1.0) 
                                                (bigfloat:+ a b 2.0))))
     (bigfloat:- 
       (bigfloat:/ (bigfloat:* (bigfloat:to ($gamma (to a))) 
                               (bigfloat:to ($gamma (to b))))
                   (bigfloat:to ($gamma (to (bigfloat:+ a b)))))
       (beta-incomplete b a (bigfloat:- 1.0 z))))
    (t
     (when *debug-gamma*
       (format t "~&BETA-INCOMPLETE enters continued fractions~%"))
     (do* ((beta-maxit *beta-incomplete-maxit*)
           (beta-eps   *beta-incomplete-eps*)
           (beta-min   (bigfloat:* beta-eps beta-eps))
           (ab (bigfloat:+ a b))
           (ap (bigfloat:+ a 1.0))
           (am (bigfloat:- a 1.0))
           (c  1.0)
           (d  (bigfloat:- 1.0 (bigfloat:/ (bigfloat:* z ab) ap)))
           (d  (if (bigfloat:< (bigfloat:abs d) beta-min) beta-min d))
           (d  (bigfloat:/ 1.0 d))
           (h  d)
           (aa 0.0)
           (de 0.0)
           (m2 0)
           (m 1 (+ m 1)))
          ((> m beta-maxit)
           (merror "Continued fractions failed in `beta_incomplete'"))
       (setq m2 (+ m m))
       (setq aa (bigfloat:/ (bigfloat:* m z (bigfloat:- b m))
                            (bigfloat:* (bigfloat:+ am m2)
                                        (bigfloat:+ a m2))))
       (setq d  (bigfloat:+ 1.0 (bigfloat:* aa d)))
       (when (bigfloat:< (bigfloat:abs d) beta-min) (setq d beta-min))
       (setq c (bigfloat:+ 1.0 (bigfloat:/ aa c)))
       (when (bigfloat:< (bigfloat:abs c) beta-min) (setq c beta-min))
       (setq d (bigfloat:/ 1.0 d))
       (setq h (bigfloat:* h d c))
       (setq aa (bigfloat:/ (bigfloat:* -1 
                                        (bigfloat:+ a m) 
                                        (bigfloat:+ ab m) z) 
                            (bigfloat:* (bigfloat:+ a m2) 
                                        (bigfloat:+ ap m2))))
       (setq d (bigfloat:+ 1.0 (bigfloat:* aa d)))
       (when (bigfloat:< (bigfloat:abs d) beta-min) (setq d beta-min))
       (setq c (bigfloat:+ 1.0 (bigfloat:/ aa c)))
       (when (bigfloat:< (bigfloat:abs c) beta-min) (setq c beta-min))
       (setq d (bigfloat:/ 1.0 d))
       (setq de (bigfloat:* d c))
       (setq h (bigfloat:* h de))
       (when (bigfloat:< (bigfloat:abs (bigfloat:- de 1.0)) beta-eps)
         (when *debug-gamma* 
           (format t "~&Continued fractions finished.~%")
           (format t "~&  z = ~A~%" z)
           (format t "~&  a = ~A~%" a)
           (format t "~&  b = ~A~%" b)
           (format t "~&  h = ~A~%" h))
         (return
          (let ((result
           (bigfloat:/ 
             (bigfloat:* h 
                         (bigfloat:expt z a)
                         (bigfloat:expt (bigfloat:- 1.0 z) b)) a)))
            (when *debug-gamma*
              (format t "~& result = ~A~%" result))
            result)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
