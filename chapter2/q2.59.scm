;; @author naoiwata
;; SICP Chapter2
;; question 2.59

(add-load-path "." :relative)
(load "pages/2.3.3.scm")

(define (union-set set1 set2)
	(cond
		((null? set1)
			set2)
		((null? set2)
			set1)
		((element-of-set? (car set1) set2)
			(union-set (cdr set1) set2))
		(else
			(cons
				(car set1)
				(union-set (cdr set1) set2)))))
; test
(define set1 (list 1 2 3 4 5))
(define set2 (list 2 4 6 8 10))

(print (element-of-set? 1 set1)) ; #t
(print (adjoin-set 11 set1)) ; (11 1 2 3 4 5)
(print (intersection-set set1 set2)) ; (2 4)
(print (union-set set1 set2)) ; (1 3 5 2 4 6 8 10)

; END