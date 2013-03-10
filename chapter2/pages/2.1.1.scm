;;
;; @author naoiwata
;; SICP Chapter2
;; 2.1.1 Example: Arithmetic Operations for Rational Numbers
;;

(define (add-rat x y)
	(make-rat 
		(+ 
			(* (numer x) (denom y))
			(* (numer y) (denom x)))
		(* (denom x) (denom y))))

(define (sub-rat x y)
	(make-rat 
		(- 
			(* (numer x) (denom y))
			(* (numer y) (denom x)))
		(* (denom x) (denom y))))

(define (mul-rat x y)
	(make-rat 
		(* (numer x) (numer y))
		(* (denom x) (denom y))))

(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
		(* (denom x) (numer y))))

(define (equal-rat? x y)
	(= (* (numer x) (denom y))
		(* (numer y) (denom x))))

; pair
(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(print (car x)) ; 1
(print (cdr x)) ; 2
(print (car (car z))) ; -> (car x) -> 1
(print (car (cdr z))) ; -> (car y) -> 3

; Representing rational numbers
(define (make-rat n d)
	(cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x)))

(define (one-half) (make-rat 1 2))
(print-rat (one-half))
; 1/2

(define (one-third) (make-rat 1 3))
(print-rat (one-third))
; 1/3

(print-rat (add-rat (one-half) (one-third)))
; 5/6

(print-rat (mul-rat (one-half) (one-third)))
; 1/6

(print-rat (add-rat (one-third) (one-third)))
; 6/9

(define (gcd a b)
	(if (= b 0)
		a
		(gcd a (remainder a b))))

(define (make-rat n d)
	(let
		((g (gcd n d)))
		(cons (/ n g) (/ d g))))

(print-rat (add-rat (one-third) (one-third)))
; 1/3/2 -> 2/3

; END
