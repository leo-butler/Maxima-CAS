;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@ed.ac.uk) 2011
;; Released under the terms of GPLv3+

(rtest:do-and-report-tests
(do-scans (a b c d "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	     (list a b c d))
  nil)
(nil nil nil nil)

(let (e)
  (do-scans (a b c d "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	       e)
    (push (list a b c d) e)))
((22 24 #(22 23) #(23 24)) (8 10 #(8 9) #(9 10)))


(let (nl)
  (do-scans (ms me rs re "([0-9]+-[0-9]+|[0-9]+)(,|;| )?" "1,2,3-6,7" (reverse nl))
    (push (list ms me rs re) nl)))
((0 2 #(0 1) #(1 2)) (2 4 #(2 3) #(3 4)) (4 8 #(4 7) #(7 8))
 (8 9 #(8) #(9)))

(let (e)
  (do-scans-to-strings (mm rr "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			   (reverse e))
    (push (list mm rr) e)))
(("ag" #("a" "g")) ("ac" #("a" "c")))

(do-scans-to-strings (mm rr "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			 nil)
  (format t "~a~%" (list mm rr)))
nil

(do-matches (a b "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
	       (list a b))
  (format t "~a~%" (list a b)))
(nil nil)

(let (c)
  (do-matches (a b "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
		 (reverse c))
    (push (list a b) c)))
((8 10) (22 24))

(let (c)
  (do-matches-as-strings (a "(a+)([a-z])" "a while ago in 200bc, ac unicorn "
			    (reverse c))
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
