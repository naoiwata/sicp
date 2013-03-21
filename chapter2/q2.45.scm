;; @author naoiwata
;; SICP Chapter2
;; question 2.45

(define right-split (split beside below))
(define up-split (split below beside))

(define (split a b)
	(if (= n 0)
		painter
		(let
			((smaller (splite painter (- n 1))))
			(a painter (b smaller smaller)))))

; END