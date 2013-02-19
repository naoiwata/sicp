;
; @author naoiwata
; SICP Chapter1
; q-1.3
;

(define (square n)
	(* n n))

(define (sum-square x y)
	(+ (square x)
	   (square y)))

(define (fact a b c)
	(cond (and (< a b) (< a c)
		  (sum-square b c))
		  (and (< b a) (< b c)
		  (sum-square a c))
		  (and (< c a) (< c b)
		  (sum-square a b))
		  (and (= a b) (< c a)
		  (sum-square a b))
		  (and (= b c) (< a b)
		  (sum-square b c))
		  (and (= a c) (< b a)
		  (sum-square a c))
		  (and (= a b c) 
		  (sum-square a b))
	(else (-1))))

; example
(print (fact 5 5 5))
