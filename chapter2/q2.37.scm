;; @author naoiwata
;; SICP Chapter2
;; question 2.37

#|
| 1 2 3 4 |
| 4 5 6 6 |
| 6 7 8 9 |
is equal to 
((1 2 3 4) (4 5 6 6) (6 7 8 9))
|#

(add-load-path "." :relative)
(load "q2.36.scm")

(define (dot-product v w)
	(accumulate 
		+ 
		0 
		(map * v w)))
;test
(print (dot-product (list 1 2 3 4) (list 5 6 7 8))) ; 70
; 1*5 + 2*6 + 3*7 + 4*8 = 5 + 12 + 21 + 32

(define (matrix-*-vector m v)
	(map
		(lambda (x) (dot-product x v))
		m))
; test
(print (matrix-*-vector (list (list 1 2) (list 3 4)) (list 5 6))) ; (17 39)
; ((1*5 + 2*6) (3*5 + 4*6)) = (17 39)

(define (transpose mat)
	(accumulate-n cons () mat))
; test
(print (transpose (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9))))
; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(define (matrix-*-matrix m n)
	(let 
		((cols (transpose n)))
		(map
			(lambda (x)
				(matrix-*-vector cols x))
			m)))

; test
(print (matrix-*-matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list (list 1 0 0) (list 0 1 0) (list 0 0 1))))
; ((1 2 3) (4 5 6) (7 8 9))

; END