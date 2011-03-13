;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

(load "gcl-regex.lisp")
(pushnew :gcl-regex *features*)
(load "cl-ppcre-interface.lisp")
(load "api.lisp")
(load "../../tests/rtest.lisp")
(in-package :gcl-regex)
(load "../../tests/rtest-api.lisp")


;; end of rtest-run.lisp 
