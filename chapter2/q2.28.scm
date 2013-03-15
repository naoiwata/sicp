;; @author naoiwata
;; SICP Chapter2
;; question 2.28

(define (fringe items)
	(cond
		((null? items)
			())
		((pair? items)
			(append
				(fringe (car items))
				(fringe (cdr items))))
		(else
			(list items))))

; test
(define x (list (list 1 2) (list 3 4)))
(print (fringe x)) ; (1 2 3 4)
(print (fringe (list x x))) ; (1 2 3 4 1 2 3 4)

; END