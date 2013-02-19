;
; @author naoiwata
; SICP Chapter1
; q-1.8
;

; (sqrt n)
(define (sqrt x)
	(sqrt-iter 2.0 1.0 x))

(define (sqrt-iter pre-guess guess x)
	(if (good-enough? pre-guess guess)
			guess
			(sqrt-iter guess
					    (improve guess x)
					    x)))

(define (good-enough? pre-guess guess)
	(< (abs (- guess pre-guess)) 0.001))

(define (improve y x)
	(/ 
		(+ 
			(/ x (* y y)) 
			(* 2 y)) 
		3))

(define (avarage a b)
	(/ (+ a b) 2))

(define (abs x)
	((if (< x 0)
		 -
		 +)
		x))

; example
(print (sqrt 5))
; 1.709975950782189