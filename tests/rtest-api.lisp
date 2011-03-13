;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

(rtest:do-and-report-tests
(do-scans (a b c d "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	     ((list a b c d)))
  nil)
(22 24 #(22 23) #(23 24))

(let (e)
  (do-scans (a b c d "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	       (e))
    (push (list a b c d) e)))
((22 24 #(22 23) #(23 24)) (8 10 #(8 9) #(9 10)))

(let (e)
  (do-scans-to-strings (mm rr "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			   ((reverse e)))
    (push (list mm rr) e)))
(("ag" #("a" "c")) ("ac" #("a" "c")))

(do-scans-to-strings (mm rr "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			 ((list mm rr)))
  (format t "~a~%" (list mm rr)))
("ac" #("a" "c"))

(do-matches (a b "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	       ((list a b)))
  (format t "~a~%" (list a b)))
(22 24)

(let (c)
  (do-matches (a b "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
		 ((reverse c)))
    (push (list a b) c)))
((8 10) (22 24))

(let (c)
  (do-matches-as-strings (a "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			    ((reverse c)))
    (push a c)))
("ag" "ac")

(register-groups-bind (first second third fourth)
    ("((a)|(b)|(c))+" "abababc" :sharedp t)
  (list first second third fourth))
("c" "a" "b" "c")

(let (c)
  (do-register-groups (a b) ("(a+)([a-z])" "a while ago in 200bc, ac unicorn ")
    (push (list a b) c))
  c)
(("a" "c") ("a" "g"))
)


;; end of rtest-api.lisp 
