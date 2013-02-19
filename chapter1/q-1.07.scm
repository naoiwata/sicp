;
; @author naoiwata
; SICP Chapter1
; q-1.7
;

; (sqrt n)
(define (sqrt x)
	(sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
	(if (good-enough? guess x)
			guess
			(sqrt-iter (improve guess x)
					   x)))

(define (good-enough? guess x)
	(< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
	(avarage guess (/ x guess)))

(define (avarage a b)
	(/ (+ a b) 2))

(define (square n)
	(* n n))

(define (abs x)
	((if (< x 0)
		 -
		 +)
		x))

; very small number
(print (sqrt 0.0001))
; 0.03230844833048122
; correct solution 0.01

; very large number
;(print (sqrt 10000000000000))
; stack overflow
; correct solution 3162277.66


; solution for solving this problem
(define (sqrt2 x)
	(sqrt-iter2 2.0 1.0 x))

(define (sqrt-iter2 pre-guess guess x)
	(if (good-enough2? pre-guess guess)
			guess
			(sqrt-iter2 guess
					    (improve guess x)
					    x)))

(define (good-enough2? pre-guess guess)
	(< (abs (- guess pre-guess)) 0.001))

; in this method, the answer will be
(print (sqrt2 5))
;2.236067977499978
(print (sqrt2 0.0001))
;0.010000714038711746
(print (sqrt2 10000000000000))
;3162277.6601683795
