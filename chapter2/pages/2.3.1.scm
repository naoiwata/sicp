;; @author naoiwata
;; SICP Chapter2
;; 2.3.1 Quotation

(print 
	(list 'a 'b) ; (a b)
	(car '(a b c)) ; a
	(cdr '(a b c))) ; (b c) 

(define (memq item x)
	(cond 
		((null? x) false)
		((eq? item (car x)) x)
		(else (memq item (cdr x)))))
; test
(print (memq 'apple '(pear banana prune)))
; (memq 'apple '(pear banana prune))
; -> (memq 'apple '(banana prune))
; -> #f

(print (memq 'apple '(x (apple sauce) y apple pear)))
; (memq 'apple '(x (apple sauce) y apple pear))
; -> (memq 'apple '((apple sauce) y apple pear))
; -> (memq 'apple '(y apple pear))
; -> (memq 'apple '(apple pear))
; -> ('apple 'pear)

; END