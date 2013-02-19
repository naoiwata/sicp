;
; @author naoiwata
; SICP Chapter1
; q-1.4
;

(define (a-plus-abs-b a b)
	((if (> b 0)
		 +
		 -)
	a
	b))

; as this shows that,
; 0 < b  : this procedure will evaluate (+ a b)
; b <= 0 : this procedure will evaluate (- a b)

; example
(print (a-plus-abs-b 3 -6))
; 9
(print (a-plus-abs-b 3 6))
; 9