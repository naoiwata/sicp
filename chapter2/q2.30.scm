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
(add-load-path "." :relative)
(load "pages/lib.scm")
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
		(lambda (x) (square x))))
; (10 (20 (30 40) 50) (60 70))

(define (square-tree tree)
	(abstruct-tree
		tree
		(lambda (x) (square x))))
; test
(print
	(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
; (1 (4 (9 16) 25) (36 49))

; ADDED
; Define square-tree both directly (i.e., without using any higher-order procedures) and also by using map and recursion.
(define (square-tree-direct tree)
	(cond
		((null? tree)
			())
		((not (pair? tree))
			(* tree tree))
		(else
			(cons
				(square-tree-direct (car tree))
				(square-tree-direct (cdr tree))))))
; test
(print
	(square-tree-direct (list 1 (list 2 (list 3 4) 5) (list 6 7))))
; (1 (4 (9 16) 25) (36 49))

(define (square-tree-usingmap tree)
	(map
		(lambda (sub-tree)
			(if (pair? sub-tree)
				(square-tree-usingmap sub-tree)
				(* sub-tree sub-tree)))
		tree))
; test
(print
	(square-tree-usingmap (list 1 (list 2 (list 3 4) 5) (list 6 7))))
; (1 (4 (9 16) 25) (36 49))

; END