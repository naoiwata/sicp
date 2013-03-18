;; @author naoiwata
;; SICP Chapter2
;; question 2.41

(add-load-path "." :relative)
(load "pages/2.2.3.scm")

(define (unique-trios n)
	(flatmap
		(lambda (i)
			(flatmap
				(lambda (j)
					(map
						(lambda (k)
							(list i j k))
					(enumerate-interval 1 (- j 1))))
			(enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))
; test
(print (unique-trios 6))
; ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3) (6 2 1) (6 3 1) (6 3 2) (6 4 1) (6 4 2) (6 4 3) (6 5 1) (6 5 2) (6 5 3) (6 5 4))

(define (sum-trios trios)
	(+
		(car trios)
		(car (cdr trios))
		(car (cdr (cdr trios)))))
; test
(print (sum-trios (list 2 3 4))) ; 9

(define (find-unique-trios-to-s n s)
	(filter
		(lambda (x)
			(= (sum-trios x) s))
		(unique-trios n)))
; test
(print (find-unique-trios-to-s 6 10))
; ((5 3 2) (5 4 1) (6 3 1))

; END