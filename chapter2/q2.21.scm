;; @author naoiwata
;; SICP Chapter2
;; question 2.21
;;

; copy
(define (scale-list items factor)
	(if (null? items)
		()
		(cons 
			(* (car items) factor)
			(scale-list (cdr items) factor))))

(print (scale-list (list 1 2 3 4 5) 10))
; (10 20 30 40 50)

(define (map proc items)
	(if (null? items)
		()
		(cons 
			(proc (car items))
			(map proc (cdr items)))))

(print (map abs (list -10 2.5 -11.6 17)))
; (10 2.5 11.6 17)

(print (map 
	(lambda (x) (* x x))
		(list 1 2 3 4)))
; (1 4 9 16)

(define (scale-list items factor)
	(map 
		(lambda (x) (* x factor))
		items))

; solution
(define (square x)
	(* x x))

(define (square-list items)
	(if (null? items)
		()
		(cons 
			(square (car items))
			(map square (cdr items)))))

(print (square-list (list 2 4 6))) ; (4 16 36)

(define (square-list items)
	(map
		(lambda (x) (square x))
		items))

(print (square-list (list 2 4 6))) ; (4 16 36)

; END