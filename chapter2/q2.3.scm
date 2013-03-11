;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.3
;;

(add-load-path "." :relative)
(load "q2.2.scm")
(load "pages/lib.scm")

; rectangle
(define (rectangle p q)
	(cons p q))

; abstruct these procedure
(define (abst-2points f r)
	(let
		((p (start-segment r))
		(q (end-segment r)))
		(f p q)))

; test : midpoint-segment
(define p1 (make-point 2 6))
(define p2 (make-point 4 10))
(define r1 (make-segment p1 p2))

(print-point 
	(abst-2points
		(lambda (p q)
			(make-point
				(average (x-point p) (x-point q))
				(average (y-point p) (y-point q))))
		r1))
;(3, 8) -> ok

; permineter
(define (permineter r)
	(abst-2points
		(lambda (p q)
			(*
				2
				(+
					(abs
						(-
							(x-point p)
							(x-point q)))
					(abs (-
							(y-point p)
							(y-point q))))))
		r))

; test
(print (permineter r1))
; 12 -> ok

; area
(define (area r)
	(abst-2points
		(lambda (p q)
			(*
				(abs
					(-
						(x-point p)
						(x-point q)))
				(abs (-
						(y-point p)
						(y-point q)))))
		r))
; test
(print (area r1))
; 8 -> ok

; END