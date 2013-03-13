;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.13
;;

; n = (100 * w)/(a + w)
; -> n = 100 * (A - a)/2 / (A + a)/2
; -> n = 100 * (A - a) / (A + a)

; a(X , x) = (X - x)/(X + x)*100
; b(Y , y) = (Y - y)/(Y + y)*100

; a(X , x) + b(Y , y)
; = (X - x)/(X + x)*100 + (Y - y)/(Y + y)*100
; = 100 * 2*(XY - xy) / (XY + Xy + xY + xy)
; ?
; = 100 * 2*(XY - xy) / 2*(XY + xy)
; = 100 * (XY - xy) / (XY + xy)
; = ab(XY, xy)
; in conclusion,
; -> a(X , x) + b(Y , y) = ab(XY, xy)

; a(+ +) b(+ +)
(define (mul-interval x y)
	(let
		((p1 (lower-bound a))
		(p2 (upper-bound a))
		(p3 (lower-bound b))
		(p4 (lower-bound b)))
		(make-interval (* p1 p3) (* p2 p4))))

; END