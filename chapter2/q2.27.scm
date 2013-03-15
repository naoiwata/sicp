;; @author naoiwata
;; SICP Chapter2
;; question 2.27

(add-load-path "." :relative)
(load "q2.18.scm")

(define x (list (list 1 2) (list 3 4)))

(print (reverse x))
; ((3 4) (1 2))

(define (deep-reverse items)
	(if (pair? items)
		(reverse (map deep-reverse items))
		items))

; test
(print (deep-reverse x))
; ((4 3) (2 1))

; END