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
; -> (fringe ((1 2) (3 4)))
; -> (append (fringe (1 2)) (fringe (3 4)))
; -> (append ((fringe 1) (fringe 2)) ((fringe 3) (fringe 4)))
; -> (append (append (list 1) (list 2)) (append (list 3) (list 4)))
; -> (append (list 1 2) (list 3 4))
; -> (list 1 2 3 4)
; -> (1 2 3 4)
(print (fringe (list x x))) ; (1 2 3 4 1 2 3 4)

; END