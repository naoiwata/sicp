;; @author naoiwata
;; SICP Chapter2
;; question 2.23

(define (for-each proc items)
	(cond 
		((null? items) 
			#f)
		(else
			(proc (car items))
			(for-each proc (cdr items)))))

(for-each 
	(lambda (x) (newline) (display x))
	(list 57 321 88))
; 57
; 321
; 88

; END