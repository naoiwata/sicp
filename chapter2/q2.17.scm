;; @author naoiwata
;; SICP Chapter2
;; question 2.17
;;

(define (last-pair items)
	(if (null? (cdr items))
		(car items)
		(last-pair (cdr items))))

; test
(print (last-pair (list 23 72 149 34))) ; 34

; END