;;
;; @author naoiwata
;; SICP Chapter1
;; 1.3.4 Newton's method
;;

(add-load-path "." :relative)
(load "lib.scm")
(load "p39.scm")

; Dg(x) = (g(x + dx) - g(x))/dx
(define dx 0.00001)
(define (deriv g)
	(lambda (x)
		(/
			(- (g (+ x dx))
			(g x))
		dx)))

(print ((deriv cube) 5))
; 75.00014999664018

(define (newton-transform g)
	(lambda (x)
		(-
			x
			(/ (g x) ((deriv g) x)))))

(define (newton g guess)
	(fixed-point (newton-transform g) guess))

; g(x) = y^2 - x = 0
(define (sqrt x)
	(newton
		(lambda (y)
			(- (square y) x))
		1.0))

; test
(print (sqrt 2))
; 1.4142135623822438

; END