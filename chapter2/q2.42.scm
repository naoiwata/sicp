;; @author naoiwata
;; SICP Chapter2
;; question 2.42

(add-load-path "." :relative)
(load "pages/2.2.3.scm")

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions)
					(safe? k positions))
				(flatmap
					(lambda (rest-of-queens)
						(map 
							(lambda (new-row)
								(adjoin-position new-row k rest-of-queens))
						(enumerate-interval 1 board-size)))
					(queen-cols (- k 1))))))
	(queen-cols board-size))

(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
	(cons new-row rest-of-queens))

(define (safe? k positions)
	(define (next new-row positions n)
		(if (null? positions)
			#t
			(let
				((this-row (car positions)))
				(if 
					(or 
						(= this-row new-row)
						(= (abs (- this-row new-row)) n))
					#f
					(next new-row (cdr positions) (+ 1 n))))))
	(next (car positions) (cdr positions) 1))

; test
; (print (queens 8))
(print (length (queens 8))) ; 92

; END