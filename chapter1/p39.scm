;;
;; @author naoiwata
;; SICP Chapter1
;; 1.3.3 finding fixed points of functions
;;

(add-load-path "." :relative)
(load "lib.scm")

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) 0.00001))
	(define (try guess)
		(let
			((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

; test1
(print 
	(fixed-point cos 1.0))
; 0.7390822985224023

; test2-1
; y |-> x/y
(define (sqrt-1 x)
	(fixed-point 
		(lambda (y) (/ x y)) 1.0))

; don't use this!
; (print (sqrt-1 2))

; test2-1
; y |-> (y + x/y)/2
(define (sqrt-2 x)
	(fixed-point
		(lambda (y) (average y (/ x y)))
		1.0))

(print 
	(sqrt-2 2))
; 1.4142135623746899

; END