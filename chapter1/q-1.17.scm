;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.17
;;

(add-load-path "." :relative)
(load "lib.scm")

; (* a 2)
(define (double a)
	(+ a a))

; (/ a 2)
(define (halve a)
	(/ a 2))

; my-multi
(define (my-multi a b)
	(cond
		((= b 0)
			0)
		((even? b)
			(my-multi
				(double a)
				(halve  b)))
		(else
			(+
				a
				(my-multi
					a
					(- b 1))))))

; test
(print (my-multi 7 8))
; 56

;; END