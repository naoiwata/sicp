;; @author naoiwata
;; SICP Chapter2
;; question 2.35

(add-load-path "." :relative)
(load "pages/2.2.3.scm")

(define (count-leaves t)
	(accumulate
		+
		0
		(map
			(lambda (x)
				(cond 
					((null? x)
						0)
					((not (pair? x))
						1)
					(else
						count-leaves x)))
			t)))
; test
(print (count-leaves (list 2 4 8))) ; 3

; END