;;
;; @author naoiwata
;; SICP Chapter2
;; 2.2.1 Representing Sequences
;;

(define one-four
	(list 1 2 3 4))
(print one-four) ; (1 2 3 4)

(print (car one-four)) ; 1

(print (cdr one-four)) ; (2 3 4)

(print (cons 10 one-four)) ; (10 1 2 3 4)

; List operations
(define (list-ref items n)
	(if (= n 0)
		(car items)
		(list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(print (list-ref squares 3)) ; 16

(define (length items)
	(if (null? items)
		0
		(+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
(define empty ())
(define one (list ()))

(print (length odds)) ; 4
(print (length empty)) ; 0
(print (length one)) ; 1

(define (length items)
	(define (length-iter a count)
		(if (null? a)
			count
			(length-iter (cdr a) (+ count 1))))
	(length-iter items 0))
(print (length odds)) ; 4

(print (append squares odds)) ; (1 4 9 16 25 1 3 5 7)
(print (append odds squares)) ; (1 3 5 7 1 4 9 16 25)
(print (append odds empty)) ; (1 3 5 7)
(print (append odds one)) ; (1 3 5 7 ())

(define (append list1 list2)
	(if (null? list1)
		list2)
		(cons (car list1) (append (cdr list1) list2)))

(print (append squares odds)) ; (1 4 9 16 25 1 3 5 7)

; END