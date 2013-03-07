;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.46
;;

(add-load-path "." :relative)
(load "lib.scm")
(load "p39.scm")

(define (iteractive-improve improve-enough? improve)
	(lambda (guess)
		(let iter
			((guess guess))
			(if (improve-enough? guess)
				guess
				(iter (improve guess))))))

; 1.1.7 sqrt
(define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x) x)))

(define (improve guess x)
	(average guess (/ x guess)))

(define (good-enough? guess x)
	(< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
	(sqrt-iter 1.0 x))

; improved sqrt
(define (improved-sqrt x)
	((iteractive-improve 
		(lambda (guess)
			(< (abs (- (square guess) x)) 0.001))
		(lambda (guess)
			(average guess (/ x guess))))
	1.0))

; test
(print (improved-sqrt 2))
; 1.4142156862745097

; 1.3.3 fixed-point -> p39.scm
;improved fixed-point
(define (improved-fixed-point f first-guess)
	((iteractive-improve
		(let 
			((next (f guess)))
			(lambda (guess)
				(< (abs (- guess next)) 0.0001))))
	first-guess))

; test
(print 
	(fixed-point cos 1.0))
; 0.7390822985224023

; END