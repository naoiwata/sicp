;;
;; @author naoiwata
;; SICP Chapter1
;; 1.3.4 Abstractions and first-class procedures
;;

(add-load-path "." :relative)
(load "p41.scm")
(load "p42.scm")


(define (fixed-of-transform g transform guess)
	(fixed-point (transform g) guess))

(define (sqrt x)
	(fixed-of-transform
		(lambda (y)
			(/ x y))
		average-damp
		1.0))

(print (sqrt 2))
; 1.4142135623746899

(define (sqrt x)
	(fixed-of-transform
		(lambda (y)
			(- (square y) x))
		newton-transform
		1.0))

(print (sqrt 2))
; 1.4142135623822438

; END