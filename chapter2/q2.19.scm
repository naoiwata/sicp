;; @author naoiwata
;; SICP Chapter2
;; question 2.19
;;

; copy
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
	(cond 
		((= amount 0) 
			1)
		((or (< amount 0) (no-more? coin-values)) 
			0)
		(else
			(+ 
				(cc amount
					(except-first-denomination coin-values))
				(cc (- amount (first-denomination coin-values))
					coin-values)))))

; solution
(define (first-denomination lists)
	(car lists))

(define (except-first-denomination lists)
	(cdr lists))

(define (no-more? lists)
	(null? lists))

; test
(print (cc 100 us-coins)) ; 292
(print (cc 100 uk-coins)) ; 104561

; END