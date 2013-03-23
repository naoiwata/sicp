;; @author naoiwata
;; SICP Chapter2
;; question 2.56

(add-load-path "." :relative)
(load "pages/2.3.2.scm")

#|
x^1 -> '(** x 1)
x^2 -> '(** x 2)
x^3 -> '(** x 3)
|#

(define (exponentiation? x)
	(and
		(eq? (car x) '**)
		(pair? (cdr x))))

(define (base x) (addend x))

(define (exponent x) (augend x))

; d(x^a)/dx = a*x^(a-1)
(define (make-exponentiation x a)
	(cond
		((=number? a 0)
			1)
		((=number? a 1) 
			x)
		(else
			(list '** x a))))

(define (deriv exp var)
	(cond 
		((number? exp) 0)
		((variable? exp)
			(if (same-variable? exp var) 1 0))
		((sum? exp)
			(make-sum 
				(deriv (addend exp) var)
				(deriv (augend exp) var)))
		((product? exp)
			(make-sum
				(make-product 
					(multiplier exp)
					(deriv (multiplicand exp) var))
				(make-product 
					(deriv (multiplier exp) var)
					(multiplicand exp))))
		((exponentiation? exp)
			(make-product
				(exponent exp)
				(make-product
					(make-exponentiation (base exp) (make-sum (exponent exp) -1))
					(deriv (base exp) var))))
		(else
			(error "unknown expression type -- DERIV" exp))))
; test
(print (deriv '(** x 0) 'x)) ; 0
(print (deriv '(** x 1) 'x)) ; 1
(print (deriv '(** x 2) 'x)) ; (+ 2 x)
(print (deriv '(** x 6) 'x)) ; (* 6 (** x 5))
(print (deriv '(+ (** x 3) (* 4 (** x 2))) 'x)) ; (* 6 (** x 5))

; END