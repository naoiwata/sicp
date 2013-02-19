;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.18
;;

(add-load-path "." :relative)
(load "q-1.17.scm")

(define (multi a b)
	(multi-iter 0 a b))

(define (multi-iter n a b)
	(cond
		((= b 0)
			n)
		((even? b)
			(multi-iter
				n
				(double a)
				(halve b)))
		(else
			(multi-iter
				(+ a n)
				a
				(- b 1)))))

; test
(print (multi 6 8))
; 48

;; END