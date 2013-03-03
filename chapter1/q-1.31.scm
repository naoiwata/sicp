;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.31
;;

(add-load-path "." :relative)
(load "lib.scm")

; (a)
(define (product term a next b)
	(if (> a b)
		1
		(*
			(term a)
			(product term (next a) next b))))

(define (inc a)
	(+ a 1))

; factorical
(define (factorical n)
	(define (multi a)
		a)
	(product multi 1 inc n))

(print 
	(factorical 6))
; -> 720

; Wallis product
(define (wallis-pi n)
	(define (f x)
		(/
			(* 4.0 (square x))
			(- (* 4.0 (square x)) 1)))
	(* 
		2.0
		(product f 1 inc n)))

(print
	(wallis-pi 999999))
; -> 3.1415918681810604

; (b)
(define (product-i term a next b)
	(define (iter a result))
	(if (> a b)
		result
		(iter (next a) (* (term a) result)))
	(iter a 0))

; END