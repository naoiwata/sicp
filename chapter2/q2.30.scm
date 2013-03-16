;; @author naoiwata
;; SICP Chapter2
;; question 2.30

; Mapping over trees

(define (scale-tree tree factor)
	(cond
		((null? tree)
			())
		((not (pair? tree))
			(* tree factor))
		(else
			(cons
				(scale-tree (car tree) factor)
				(scale-tree (cdr tree) factor)))))

(print 
	(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10))
; (10 (20 (30 40) 50) (60 70))

(define (scale-tree tree factor)
	(map
		(lambda (sub-tree)
			(if (pair? sub-tree)
				(scale-tree sub-tree factor)
				(* sub-tree factor)))
		tree))
(print 
	(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10))
; (10 (20 (30 40) 50) (60 70))

; question
; be abstruct this map procedure
(define (abstruct-tree tree g)
	(map
		(lambda (sub-tree)
			(if (pair? sub-tree)
				(abstruct-tree sub-tree g)
				(g sub-tree)))
		tree))

; test
(print 
	(abstruct-tree 
		(list 1 (list 2 (list 3 4) 5) (list 6 7))
		(lambda (x) (* 10 x))))
; (10 (20 (30 40) 50) (60 70))

(define (square-tree tree)
	(abstruct-tree
		tree
		(lambda (x) (* x x))))
; test
(print
	(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
; (1 (4 (9 16) 25) (36 49))

; END