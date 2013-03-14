;; @author naoiwata
;; SICP Chapter2
;; question 2.20
;;

; copy
(define (f x y . z))
(print (f 1 2 3 4 5 6)) ; 3

(define (g x . z))
(print (g 1 2 3 4 5 6)) ; 2

(define (g . z))
(print (g 1 2 3 4 5 6)) ; 1

; solution
(define (name-parity x . y)
	(let iter
		((lists (list x))
		(items y))
		(if (null? items)
			lists
			(cond
				((and (even? x) (even? (car items)))
					(iter (append lists (list (car items))) (cdr items)))
				((and (odd? x) (odd? (car items)))
					(iter (append lists (list (car items))) (cdr items)))
				(else
					(iter lists (cdr items)))))))

; test
(print (name-parity 1 2 3 4 5 6 7)) ; (1 3 5 7)
(print (name-parity 2 3 4 5 6 7)) ; (2 4 6)

; END