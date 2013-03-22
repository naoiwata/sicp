;; @author naoiwata
;; SICP Chapter2
;; question 2.54

(define (equal? seq-a seq-b)
	(cond
		((doubt pair? seq-a seq-a)
			(and
				(equal? (car seq-a) (car seq-b))
				(equal? (cdr seq-a) (cdr seq-b))))
		((not (doubt pair? seq-a seq-b))
			(eq? seq-a seq-b))
		((doubt null? seq-a seq-b)
			#t)
		(else
			#f)))

(define (doubt f a b)
	(and (f a) (f b)))

(print (equal? '(this is a list) '(this is a list))) ; #t
(print (equal? '(this is a list) '(this (is a) list))) ; #f

; END