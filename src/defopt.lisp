;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module defopt macro)

;; For defining optimizers which run on various systems.
;; Q: What is an optimizer?
;; A: A transformation which takes place in the compiler.

;; ***==> Right now, DEFOPT is used just like you would a DEFMACRO <==***
;; (defopt <name> <arlist> <body-boo>)

;; I BELIEVE THE PROBLEM WITH ECL HERE IS THAT DEFINE-COMPILER-MACRO IS NOT
;; PROCESSED WHEN FILES ARE ONLY LOADED AND NOT COMPILED; IT SEEMS PLAUSIBLE
;; THAT OTHER LISPS WOULD DEMONSTRATE THE SAME PROBLEM IF ONLY LOADED.
;; PROBABLY AN EVAL-WHEN IS NEEDED HERE.

(defmacro defopt (&rest other)
  `(#-(or gcl ecl) define-compiler-macro #+gcl si::define-compiler-macro #+ecl defmacro ,@other)) 
