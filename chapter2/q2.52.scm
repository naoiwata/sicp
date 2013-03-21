;; @author naoiwata
;; SICP Chapter2
;; question 2.52

; (a)
; can't make wave view

; (b)
(define (up-splite painter n)
	(if (= n 0)
		painter
		(below painter (up-splite painter (- n 1)))))

(define (right-splite painter n)
	(if (= n 0)
		painter
		(beside painter (right-splite painter (- n 1)))))

(define (corner-split painter n)
	(if (= n 0)
		painter
		(let 
			((up (up-split painter (- n 1)))
			(right (right-split painter (- n 1)))
			(corner (corner-split painter (- n 1))))
			(corner (corner-split painter (- n 1))))
			(beside 
				(below painter up)
				(below right corner))))

; END