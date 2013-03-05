;;
;; @author naoiwata
;; SICP Chapter1
;; 1.3.3 half-interval method
;;

(add-load-path "." :relative)
(load "lib.scm")

(define (search f neg-point pos-point)
	(let
		((mid-point (average neg-point pos-point)))
		(if (close-enough? neg-point pos-point)
			mid-point
			(let
				((test-value (f mid-point)))
				(cond
					((positive? test-value)
						(search f neg-point mid-point))
					((negative? test-value)
						(search f mid-point pos-point))
					(else
						mid-point))))))

(define (close-enough? x y)
	(< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
	(let
		((a-value (f a))
		(b-value (f b)))
		(cond
			((and (negative? a-value) (positive? b-value))
				(search f a b))
			((and (negative? b-value) (positive? a-value))
				(search f b a))
			(else
				(error "error values" a b)))))

; test-1
(print 
	(half-interval-method sin 2.0 4.0))
; 3.14111328125

; test-2
(print 
	(half-interval-method
		(lambda (x) (- (* x x x) (* 2 x) 3))
		1.0
		2.0))
; 1.89306640625

; END