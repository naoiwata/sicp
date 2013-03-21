;; @author naoiwata
;; SICP Chapter2
;; question 2.48

(define (make-segment origin end)
	(cons origin end))

(define (start-segment segment)
	(car segment))

(define (end-segment segment)
	(cdr segment))

; END