;;
;; @author naoiwata
;; SICP Chapter1
;; 1.3.1 Procedures as Arguments
;;

(define (sum term a next b)
	(if (> a b)
		0
		(+ 
			(term a)
			(sum term (next a) next b))))

; pi
(define (pi-sum a b)
	(define (pi-term x)
		(/ 1.0 (* x (+ x 2))))
	(define (pi-next x)
		(+ x 4))
	(sum pi-term a pi-next b))

(print
	(* 8 (pi-sum 1 1000)))
; -> 3.139592655589783

; integral of a function f between the limits a and b
(define (cube x)
	(* x x x))

(define (integral f a b dx)
	(define (add-dx x) (+ x dx))
		(* 
			(sum f (+ a (/ dx 2.0)) add-dx b)
			dx))

(print
	(integral cube 0 1 0.001))
; 0.249999875000001

