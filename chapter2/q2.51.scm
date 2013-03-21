;; @author naoiwata
;; SICP Chapter2
;; question 2.51

; beside like
(define (below painter1 painter2)
	(let
		((split-point (make-vect 0.0 0.5)))
		(let
			((paint-top
				(transform-painter painter1
					split-point
					(make-vect 1.0 0.5)
					(make-vect 0.0 1.0)))
			(paint-bottom
				(transform-painter painter2
					(make-vect 0.0 0.0)
					(make-vect 1.0 0.0)
					split-point)))
			(lambda (frame)
				(paint-top frame)
				(paint-bottom frame)))))

; rolate
(define (below painter1 painter2)
	(let
		((rotate-1 (rotate-270 painter1))
		(rotate-2 (rotate-270 painter2)))
		(let
			((rotate-12 (beside rotate-1 rotate-2)))
			(rotate90 rotate-12))))

(define (rotate90 painter)
	(transform-painter painter
		(make-vect 1.0 0.0)
		(make-vect 0.0 1.0)
		(make-vect 0.0 0.0)))))

; END