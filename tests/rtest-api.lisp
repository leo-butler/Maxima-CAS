;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

(do-scans (a b c d "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	     ((format t "~a~%" (list a b c d))))
  (values a b c d))

(let (e)
  (do-scans (a b c d "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	       (e))
    (push (list a b c d) e)))

'((22 24 #(22 23) #(23 24)) (8 10 #(8 9) #(9 10)))



(let (c)
  (do-scans-to-strings (mm rr "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			   ((reverse c)))
    (push (list mm rr) c)))

(do-scans-to-strings (mm rr "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			 ((values mm rr)))
  (format t "~a~%" (list mm rr)))


(do-matches (a b "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	       ((values a b)))
  (format t "~a~%" (list a b)))


(do-matches (a b "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	       ((values a b)))
  (format t "~a~%" (list a b)))


(do-matches-as-strings (a "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			  ((values a)))
  (format t "~a~%" (list a)))

(let (c)
  (do-matches-as-strings (a "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			    ((reverse c)))
    (push a c)))



(register-groups-bind (first second third fourth)
		      ("((a)|(b)|(c))+" "abababc" :sharedp t)
		      (list first second third fourth))
'("c" "a" "b" "c")




;; end of rtest-api.lisp 
