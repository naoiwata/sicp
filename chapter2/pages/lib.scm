;;
;; @author naoiwata
;; SICP Chapter2
;; lib on chapter2
;;

; abs
(define (abs n)
	(if (< n 0)
		(- 0 n)
		n))

; average
(define (average a b)
	(/
		(+ a b)
		2))

; END