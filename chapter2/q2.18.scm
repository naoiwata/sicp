;; @author naoiwata
;; SICP Chapter2
;; question 2.18
;;

(define (reverse items)
	(define (iter lists a)
		(if (null? a)
			lists
			(iter (cons (car a) lists) (cdr a))))
	(iter () items))

; test
(print (reverse (list 1 4 9 16 25)))
; (25 16 9 4 1)

; END