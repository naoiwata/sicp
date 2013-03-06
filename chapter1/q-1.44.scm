;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.44
;;

(add-load-path "." :relative)
(load "p42.scm")
(load "q-1.43.scm")

(define (smooth f)
	(lambda (x)
		(/
			(+
				(f (- x dx))
				(f x)
				(f (+ x dx)))
			3)))

(define (n-fold-smoothed-function f n)
	((repeated smooth n) f))

; END