;; @author naoiwata
;; SICP Chapter2
;; question 2.58

(add-load-path "." :relative)
(load "q2.57.scm")

(define (sum? x)
	(and (pair? x) (eq? (cadr x) '+)))

(define (addend s) 
	(car s))

(define (product? x)
	(and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) 
	(car p))

(define (make-sum a1 a2)
	(cond 
		((=number? a1 0) 
			a2)
		((=number? a2 0) 
			a1)
		((and (number? a1) (number? a2)) 
			(+ a1 a2))
		(else 
			(list a1 '+ a2))))

(define (make-product m1 m2)
	(cond 
		((or (=number? m1 0) (=number? m2 0))
			0)
		((=number? m1 1) 
			m2)
		((=number? m2 1) 
			m1)
		((and (number? m1) (number? m2)) 
			(* m1 m2))
		(else 
			(list m1 '* m2))))

; test
(print (deriv '(x + (3 * (x + (y + 2)))) 'x))
; 4
(print (deriv '(x * (y * (x + 3))) 'x))
; ((x * y) + (y * (x + 3)))

; (b)

; (cddr (list 1 2 3 4)) -> (3 4)
; (cdddr (list 1 2 3 4)) -> (4)

(define (augend s)
	(if (null? (cdddr s))
		(caddr s)
		(cddr s)))

(define (multiplicand s)
	(if (null? (cdddr s)) 
		(caddr s)
		(cddr s)))
; test
(print (deriv '(x + 3 * (x + y + 2)) 'x))
; 4
(print (deriv '(x * y * (x + 3)) 'x))
; ((x * y) + (y * (x + 3)))

; END