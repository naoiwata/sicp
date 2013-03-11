;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.5
;;

; (2^a) * (3^b)
(define (expt x n) 
	(if (= n 0) 
		1 
		(* x (expt x (- n 1)))))

; cons
(define (cons a b)
	(*
		(expt 2 a)
		(expt 3 b)))

; car 
(define (pair-expt x n)
	(let iter
		((i 0)
		(k x))
		(if (= (remainder k n) 0)
			(iter (+ i 1) (/ k n))
			i)))

(define (car x)
	(pair-expt x 2))

; cdr
(define (cdr x)
	(pair-expt x 3))

; test
(define test (cons 4 2))
; cons
(print test) ; 2^4*3^2 = 16*9 = 144
; car
(print (car test)) ; 4
; cdr
(print (cdr test)) ; 2

; END

