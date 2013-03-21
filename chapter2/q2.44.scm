;; @author naoiwata
;; SICP Chapter2
;; question 2.44

(define (up-splite painter n)
	(if (= n 0)
		painter
		(let
			((smaller (up-splite painter (- n 1))))
			(below painter (beside smaller smaller)))))

; END