;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.4
;;

(define (cons x y)
	(lambda (m)
		(m x y)))

(define (car z)
	(z
		(lambda (p q)
			p)))

; (define z (cons x y))
; (car z)
; -> (car (cons x y))
; -> (car (lambda (m) (m x y)))
; -> ((lambda (m) (m x y)) (lambda (p q) p)))
; -> ((lambda (p q) p) x y)
; -> (lambda (x y) x)
; -> x

(define (cdr z)
	(z
		(lambda (p q)
			q)))

; test
(define test
	(cons 3 7))

(print (car test))
; 3
(print (cdr test))
; 7

; END