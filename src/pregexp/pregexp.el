(fset 'pregexp-reload-all
      "(load \"pregexp.lisp\")(load \"tester.lisp\")(load \"cl-ppcre-interface.lisp\")(load \"api.lisp\")(in-package :pregexp)\C-m")

(fset 'pregexp-run-tests
   "\C-c\C-lpreg\C-i-te\C-i\C-m")

(fset 'pregexp-start-sbcl
      "\C-u-\C-[xslime\C-msbcl\C-m")

(fset 'pregexp-set-slime-dir
      ",cd\C-m\C-a\C-k~/maxim\C-isan\C-igi\C-imax\C-i-\C-isr\C-ipre\C-i\C-m\C-[xpregexp-reload-all\C-m")

