;; @author naoiwata
;; SICP Chapter2
;; question 2.39

(add-load-path "." :relative)
(load "q2.38.scm")

(define (reverse-r sequence)
	(fold-right
		(lambda (x y)
			(append y (list x)))
		()
		sequence))
; test
(print (reverse-r (list 1 2 3 4))) ; (4 3 2 1)

(define (reverse-l sequence)
	(fold-left 
		(lambda (x y)
			(cons y x))
		()
		sequence))
; test
(print (reverse-r (list 1 2 3 4))) ; (4 3 2 1)

; END