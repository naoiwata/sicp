;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.11
;;

; a(- -) b(- -) - A
; a(- -) b(- +) - B
; a(- -) b(+ +) - C
; a(+ +) b(- -) - D
; a(+ +) b(- +) - E
; a(+ +) b(+ +) - F
; a(- +) b(- -) - G
; a(- +) b(- +) - H
; a(- +) b(+ +) - I

(define (mul-interval x y)
	(let
		((p1 (lower-bound a))
		(p2 (upper-bound a))
		(p3 (lower-bound b))
		(p4 (lower-bound b)))
		(cond
			((< p2 0)
				(cond
					((< 0 p3) (make-interval (* p1 p4) (* p2 p3))) ; - A
					((< p4 0) (make-interval (* p2 p4) (* p1 p3))) ; - B
					(else     (make-interval (* p1 p4) (* p1 p4))))) ; - C
			((< 0 p1)
				(cond
					((< 0 p3) (make-interval (* p1 p3) (* p2 p4))) ; - D
					((< p4 0) (make-interval (* p2 p3) (* p1 p4))) ; - E
					(else     (make-interval (* p2 p3) (* p2 p4))))) ; - F
			(else
				(cond
					((< 0 p3) (make-interval (* p1 p4) (* p2 p3))) ; - G
					((< p4 0) (make-interval (* p2 p3) (* p1 p4))) ; - H
					(else     (make-interval (min (* p1 p4) (* p2 p3)) (max (* p1 p3) (* p2 p4))))))))) ; - I

; END