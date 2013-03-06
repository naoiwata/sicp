;;
;; @author naoiwata
;; SICP Chapter1
;; 1.3.4 Procedures as Returned Values
;;

(add-load-path "." :relative)
(load "lib.scm")
(load "p39.scm")

(define (average-damp f)
	(lambda (x)
		(average x (f x))))

(define (sqrt x)
	(fixed-point
		(average-damp
			(lambda (y) (/ x y)))
		1.0))

(print (sqrt 2))
; 1.4142135623746899

(define (cube-root x)
	(fixed-point
		(average-damp
			(lambda (y) (/ x (square y))))
		1.0))

(print (cube-root 1000))
; 10.000002544054729

; END