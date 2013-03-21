;; @author naoiwata
;; SICP Chapter2
;; question 2.46

(define (make-vect x y)
	(cons x y))

(define (xcor-vect vec)
	(car vec))

(define (ycor-vect vec)
	(cdr vec))

; add-vect
(define (add-vect a b)
	(make-vect
		(+ (xcor-vect a) (xcor-vect b))
		(+ (ycor-vect a) (ycor-vect b))))

; sub-vect
(define (sub-vect a b)
	(make-vect
		(- (xcor-vect a) (xcor-vect b))
		(- (ycor-vect a) (ycor-vect b))))

; scale-vect
(define (scale-vect a s)
	(make-vect
		(* s (xcor-vect a))
		(* s (ycor-vect a))))

; test
(define a (make-vect 1 2))
(define b (make-vect 3 4))

(print (add-vect a b)) ; (4 . 6)
(print (sub-vect a b)) ; (-2 . -2)
(print (scale-vect a 3)) ; (3 . 6)

; END