;
; @author naoiwata
; SICP Chapter1
; q-1.6
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

; expample
(print (sqrt 9))
; 3.00009155413138
(print (sqrt (+ 100 37)))
; 11.704699917758145


; when using new-if 
(define (new-if predicate then-clause else-clause)
	(cond (predicate then-clause)
		  (else else-clause)))

(print (new-if (= 2 3) 0 5))
; 5
(print (new-if (= 1 1) 0 5))
; 0

; in using 'if' sequence,
;(sqrt 2)

;(sqrt-iter 1.0 2)

;(if (< (abs (- (square 1.0) 2)) 0.001)
;		1.0
;		(sqrt-iter (improve 1.0 2)
;				   2)))

;(sqrt-iter (improve 1.0 2)
;			2))

;(sqrt-iter 1.5 2))

; ...

; however in using 'new-if' sequence,
;(sqrt 2)

;(sqrt-iter 1.0 2)

;(new-if (< (abs (- (square 1.0) 2)) 0.001)
;		1.0
;		(sqrt-iter (improve 1.0 2)
;				   2)))

;(new-if (< (abs (- (square 1.0) 2)) 0.001)
;		1.0
;		(new-if (< (abs (- (square 1.0) 2)) 0.001)
;			1.0
;			(sqrt-iter (improve 1.0 2)
;				2)))

;(new-if (< (abs (- (square 1.0) 2)) 0.001)
;		1.0
;		(new-if (< (abs (- (square 1.0) 2)) 0.001)
;			1.0
;			(new-if (< (abs (- (square 1.0) 2)) 0.001)
;				1.0
;				(sqrt-iter (improve 1.0 2)
;					2)))
; as this shows, this recursion will never stop.

; ifやcondはまず条件の手続きを評価し、その条件を満たす式のみを評価している。
; 一方、new-ifの手続きの解釈系は作用的順序の評価を行うため、new-ifのすべての引数を評価してから式が実行される。
; 引数に再帰文が含まれるので引数の評価が無限に繰り返されてしまう。