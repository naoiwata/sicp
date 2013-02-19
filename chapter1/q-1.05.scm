;
; @author naoiwata
; SICP Chapter1
; q-1.5
;

(define (p) (p))
(define (test x y)
	(if (= x 0)
		0
		y))
;(test 0 (p))

; when in normal-order evaluation is used,
; (test 0 (p))

; (if (= 0 0)
;		0
;		y))

; 0
; so '0' will be returned


; when in applicative-order evaluation is used,
; (test 0 (p))

; (test 0 (p))

; this procedure will not be stopped.