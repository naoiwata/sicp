;; @author naoiwata
;; SICP Chapter2
;; 2.2.4 Example: A Picture Language

; △▽
(define wave2 (beside wave (flip-vert wave)))

; △▽
; △▽
(define wave4 (below wave2 wave2))

; flipped-pairs = wave4
(define (flipped-pairs painter)
	(let 
		((painter2 (beside painter (flip-vert painter))))
		(below painter2 painter2)))

(define wave4 (flipped-pairs wave))

; recursive
(define (right-split painter n)
	(if (= n 0)
	painter
	(let 
		((smaller (right-split painter (- n 1))))
		(beside painter (below smaller smaller)))))

(define (corner-split painter n)
	(if (= n 0)
		painter
		(let 
			((up (up-split painter (- n 1)))
			(right (right-split painter (- n 1))))
			(let 
				((top-left (beside up up))
				(bottom-right (below right right))
				(corner (corner-split painter (- n 1))))
				(beside 
					(below painter top-left)
					(below bottom-right corner))))))

(define (square-limit painter n)
	(let 
		((quarter (corner-split painter n)))
		(let 
			((half (beside (flip-horiz quarter) quarter)))
			(below (flip-vert half) half))))

;; Higher-order operations ;;

; tl:↑←copy tr:↑→copy bl:↓←copy br:↓→copy
(define (square-of-four tl tr bl br)
	(lambda (painter)
		(let 
			((top (beside (tl painter) (tr painter)))
			(bottom (beside (bl painter) (br painter))))
			(below bottom top))))

(define (flipped-pairs painter)
	(let 
		((combine4 (square-of-four identity flip-vert
identity flip-vert)))
		(combine4 painter)))

(define (square-limit painter n)
	(let 
		((combine4 (square-of-four flip-horiz identity
rotate180 flip-vert)))
		(combine4 (corner-split painter n))))

;; Frames ;;

(define (frame-coord-map frame)
	(lambda (v)
		(add-vect
			(origin-frame frame)
			(add-vect 
				(scale-vect (xcor-vect v)
					(edge1-frame frame))
				(scale-vect (ycor-vect v)
					(edge2-frame frame))))))

((frame-coord-map a-frame) (make-vect 0 0))

(origin-frame a-frame)

;; Painters ;;

(define (segments->painter segment-list)
	(lambda (frame)
		(for-each
			(lambda (segment)
				(draw-line
					((frame-coord-map frame) (start-segment segment))
					((frame-coord-map frame) (end-segment segment))))
			segment-list)))

;; Transforming and combining painters ;;

(define (transform-painter painter origin corner1 corner2)
	(lambda (frame)
		(let 
			((m (frame-coord-map frame)))
			(let 
				((new-origin (m origin)))
				(painter
					(make-frame new-origin
						(sub-vect (m corner1) new-origin)
						(sub-vect (m corner2) new-origin)))))))

; flip painter images vertically:
(define (flip-vert painter)
	(transform-painter painter
		(make-vect 0.0 1.0) ; new origin
		(make-vect 1.0 1.0) ; new end of edge1
		(make-vect 0.0 0.0))) ; new end of edge2

; painter that shrinks its image to the upper-right quarter of the frame
(define (shrink-to-upper-right painter)
	(transform-painter painter
		(make-vect 0.5 0.5)
		(make-vect 1.0 0.5)
		(make-vect 0.5 1.0)))

; rotate images counterclockwise by 90 degrees
(define (rotate90 painter)
	(transform-painter painter
		(make-vect 1.0 0.0)
		(make-vect 1.0 1.0)
		(make-vect 0.0 0.0)))

; squash images towards the center of the frame
(define (squash-inwards painter)
	(transform-painter painter
		(make-vect 0.0 0.0)
		(make-vect 0.65 0.35)
		(make-vect 0.35 0.65)))

; beside
(define (beside painter1 painter2)
	(let 
		((split-point (make-vect 0.5 0.0)))
		(let 
			((paint-left
				(transform-painter painter1
					(make-vect 0.0 0.0)
					split-point
					(make-vect 0.0 1.0)))
			(paint-right
				(transform-painter painter2
				split-point
				(make-vect 1.0 0.0)
				(make-vect 0.5 1.0))))
			(lambda (frame)
				(paint-left frame)
				(paint-right frame)))))

; END