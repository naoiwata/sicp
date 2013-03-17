;; @author naoiwata
;; SICP Chapter2
;; question 2.40

(add-load-path "." :relative)
(load "pages/2.2.3.scm")

(define (unique-pairs n)
	(flatmap
		(lambda (i)
			(map 
				(lambda (j) 
					(list i j))
			(enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))
; test
(print (unique-pairs 6))
; ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4) (6 1) (6 2) (6 3) (6 4) (6 5))

(define (prime-sum-pairs n)
	(map
		make-pair-sum
		(filter prime-sum? (unique-pairs n))))
; test
(print (prime-sum-pairs 6))
; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

; END