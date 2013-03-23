;; @author naoiwata
;; SICP Chapter2
;; 2.3.2 Example: Symbolic Differentiation

;; The differentiation program with abstract data ;;

#|
(variable? e) ; Is e a variable?
(same-variable? v1 v2) ; Are v1 and v2 the same variable?
(sum? e) ; Is e a sum?
(addend e) ; Addend of the sum e.
(augend e) ; Augend of the sum e.
(make-sum a1 a2) ; Construct the sum of a1 and a2.
(product? e) ; Is e a product?
(multiplier e) ; Multiplier of the product e.
(multiplicand e) ; Multiplicand of the product e.
(make-product m1 m2) ; Construct the product of m1 and m2.
|#

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
		(else
			(error "unknown expression type -- DERIV" exp))))

;; Representing algebraic expressions ;;

(define (variable? x) 
	(symbol? x))

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) 
	(list '+ a1 a2))

(define (make-product m1 m2) 
	(list '* m1 m2))

(define (sum? x)
	(and (pair? x) (eq? (car x) '+)))

(define (addend s) 
	(cadr s))

(define (augend s) 
	(caddr s))

(define (product? x)
	(and (pair? x) (eq? (car x) '*)))

(define (multiplier p) 
	(cadr p))

(define (multiplicand p) 
	(caddr p))

; test
(print (deriv '(+ x 3) 'x)) 
; (+ 1 0)
(print (deriv '(* x y) 'x)) 
; (+ (* x 0) (* 1 y)) -> this should be 'y :(
(print (deriv '(* (* x y) (+ x 3)) 'x)) 
; (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

(define (make-sum a1 a2)
	(cond 
		((=number? a1 0) 
			a2)
		((=number? a2 0) 
			a1)
		((and (number? a1) (number? a2)) 
			(+ a1 a2))
		(else 
			(list '+ a1 a2))))

(define (=number? exp num)
	(and (number? exp) (= exp num)))

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
			(list '* m1 m2))))

; test
(print (deriv '(+ x 3) 'x)) 
; 1
(print (deriv '(* x y) 'x)) 
; y
(print (deriv '(* (* x y) (+ x 3)) 'x)) 
; (+ (* x y) (* y (+ x 3)))

; END