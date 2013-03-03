;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.30
;;

(define (sum term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ (term a) result))))
	(iter a 0))

; END