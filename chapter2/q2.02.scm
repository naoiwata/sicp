;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.2
;;

(add-load-path "." :relative)
(load "pages/lib.scm")

; line
(define (make-segment p q)
	(cons p q))

(define (start-segment p)
	(car p))

(define (end-segment p)
	(cdr p))

; point
(define (make-point x y)
	(cons x y))

(define (x-point p)
	(car p))

(define (y-point p)
	(cdr p))

; middle point
(define (midpoint-segment r)
	(let
		((p (start-segment r))
		(q (end-segment r)))
		(make-point
			(average (x-point p) (x-point q))
			(average (y-point p) (y-point q)))))

(define (print-point p)
	(newline)
	(display "(")
	(display (x-point p))
	(display ", ")
	(display (y-point p))
	(display ")"))

; test p(2.6) q(4.10)
(define p1 (make-point 2 6))
(define p2 (make-point 4 10))
(define r1 (make-segment p1 p2))
(print-point (midpoint-segment r1))
; (3, 8)

; END