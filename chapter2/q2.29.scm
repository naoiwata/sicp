;; @author naoiwata
;; SICP Chapter2
;; question 2.29

(define (make-mobile left right)
	(list left right))

(define (make-branch length structure)
	(list length structure))

; (a) 
(define (left-branch items)
	(car items))

(define (right-branch items)
	(car (cdr items)))

(define (branch-length items)
	(car items))

(define (branch-structure items)
	(car (cdr items)))

; (b)
(define (total-weight items)
	(let branch-weight
		((mobile (branch-structure items)))
		(cond
			((null? mobile)
				0)
			((pair? mobile)
				(total-weight mobile))
			(else
				mobile))))

; (c)
(define (balanced? items)
	(let
		((left (left-branch items))
		(right (right-branch items)))
		(cond 
			(eq?
				(* (branch-length left) (branch-structure left))
				(* (branch-length right) (branch-structure right)))
			((balanced? (branch-structure left))
			(balanced? (branch-structure right))))
		(else
			#f)))

; (d)
; if these are changed such as
(define (make-mobile left right)
	(cons left right))

(define (make-branch length structure)
	(cons length structure))

; then, only these procedures need to be changed.
(define (right-branch items)
	(cdr items))

(define (right-branch items)
	(cdr items))

; END