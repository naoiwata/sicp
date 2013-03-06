;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.39
;;

(add-load-path "." :relative)
(load "lib.scm")
(load "q-1.37.scm")

; tan(x) = x/(1 - x^2/(3 - x^2/(...)))
(define (tan-cf x k)
	(cont-franc
		(lambda (i)
			(if (= i 1)
				x
				(- (square x))))
		(lambda (i) (- (* 2 i) 1))
		k))
(newline)
(define pi 3.14111328125)

; test
(print (exact->inexact (tan-cf (/ pi 6) 100)))
; 0.5772437469163979
(print (exact->inexact (tan-cf (/ pi 4) 100)))
; 0.9997603425502442

; END