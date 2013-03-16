;; @author naoiwata
;; SICP Chapter2
;; question 2.34

(add-load-path "." :relative)
(load "pages/2.2.3.scm")

(define (horner-eval x coefficient-sequence)
	(accumulate 
		(lambda (this-coeff higher-terms)
			(+
				this-coeff
				(*
					x
					higher-terms)))
		0
		coefficient-sequence))
; test
(print (horner-eval 2 (list 1 3 0 5 0 1)))
; 1 + 3*x + 0*x^2 + 5*x^3 + 0*x^4 + 1*x^5 (x = 2)
; 1 + 3*2 + 0*4 + 5*8 + 0*16 + 1*32
; 1 + 6 + 40 + 32 
; 79

; END
