;; @author naoiwata
;; SICP Chapter2
;; question 2.31

(add-load-path "." :relative)
(load "q2.30.scm")

(define (tree-map square tree)
	(abstruct-tree
		tree
		(lambda (x) (square x))))

(define (square-tree tree)
	(tree-map square tree))

; test
(display
	(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

; test