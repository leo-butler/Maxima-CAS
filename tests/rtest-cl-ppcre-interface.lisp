;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+
#+gcl
(in-package :gcl-regex)
#+gcl
(rtest:do-and-report-tests

    ;; *nix
    (translate-special-chars "A.Z")
  "A[^
]Z"

  
  (translate-special-chars "A\\sZ")
  "A[ 	
]Z"

  (translate-special-chars "A\\SZ")
  "A[^ 	
]Z"

  (scan "." "
")
  nil

  (scan "." "a")
  0

  (scan "\\S" "a")
  0
)


;; end of rtest-cl-ppcre-interface.lisp 
