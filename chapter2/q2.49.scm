;; @author naoiwata
;; SICP Chapter2
;; question 2.49

; a. The painter that draws the outline of the designated frame.
(define a-painter
	(let
		((vect1 (make-vevt 0.0 0.0))
		(vect2 (make-vevt 1.0 0.0))
		(vect3 (make-vevt 1.0 1.0))
		(vect4 (make-vevt 0.0 1.0)))
		(segments->painter
			(list
				(make-segment vect1 vect2)
				(make-segment vect2 vect3)
				(make-segment vect3 vect4)
				(make-segment vect4 vect1))))) ; □

; b. The painter that draws an X by connecting opposite corners of the frame.
(define b-painter
	(let
		((vect1 (make-vevt 0.0 0.0))
		(vect2 (make-vevt 1.0 0.0))
		(vect3 (make-vevt 1.0 1.0))
		(vect4 (make-vevt 0.0 1.0)))
		(segments->painter
			(list
				(make-segment vect1 vect3)
				(make-segment vect2 vect4))))) ; X

; c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define a-painter
	(let
		((vect1 (make-vevt 0.5 0.0))
		(vect2 (make-vevt 1.0 0.5))
		(vect3 (make-vevt 0.5 1.0))
		(vect4 (make-vevt 0.0 0.5)))
		(segments->painter
			(list
				(make-segment vect1 vect2)
				(make-segment vect2 vect3)
				(make-segment vect3 vect4)
				(make-segment vect4 vect1))))) ; ◇

; d. The wave painter.
; i dont know the points of wave:(


; END