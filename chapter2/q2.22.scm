;; @author naoiwata
;; SICP Chapter2
;; question 2.22

(define (square x)
	(* x x))

; (1)
(define (square-list items)
	(define (iter things answer)
		(if (null? things)
			answer
			(iter 
				(cdr things)
				(cons 
					(square (car things))
					answer))))
	(iter items ()))

; test
(print (square-list (list 2 4 6))) ; (36 16 4)
; (square-list (list 2 4 6))
; -> (iter (list 2 4 6) ())
; -> (iter (4 6) (cons (* 2 2) ()))
; -> (iter (4 6) (4))
; -> (iter (6) (cons (* 4 4) (4)))
; -> (iter (6) (16 4))
; -> (iter () (cons (* 6 6) (16 4)))
; -> (iter () (36 16 4))
; -> (36 16 4)

; (2)
(define (square-list items)
	(define (iter things answer)
		(if (null? things)
			answer
			(iter 
				(cdr things)
				(cons answer
					(square (car things))))))
	(iter items ()))

; test
(print (square-list (list 2 4 6))) ; (((() . 4) . 16) . 36)
; (square-list (list 2 4 6))
; -> (iter (list 2 4 6) ())
; -> (iter (4 6) (cons () (* 2 2)))
; -> (iter (4 6) (() . 4))
; -> (iter (6) (cons (() . 4) (* 4 4)))
; -> (iter (6) ((() . 4) . 16))
; -> (iter () (cons ((() . 4) . 16) (* 6 6)))
; -> (iter () (((() . 4) . 16) . 36))
; -> (((() . 4) . 16) . 36)

; END