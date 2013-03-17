;; @author naoiwata
;; SICP Chapter2
;; 2.2.3 Sequences as Conventional Interfaces

(add-load-path "." :relative)
(load "lib.scm")

(define (sum-odd-squares tree)
	(cond 
		((null? tree) 
			0)
		((not (pair? tree))
			(if (odd? tree) 
				(square tree) 
				0))
		(else 
			(+ 
				(sum-odd-squares (car tree))
				(sum-odd-squares (cdr tree))))))
; test
(print (sum-odd-squares (list 1 2 3 4 5)))
; 1^1 + 3^3 + 5^5 = 1 + 9 + 25 = 35

(define (even-fibs n)
	(define (next k)
		(if (> k n)
			()
			(let 
				((f (fib k)))
				(if (even? f)
					(cons f (next (+ k 1)))
					(next (+ k 1))))))
			(next 0))
; test
(print (even-fibs 10)) ; (2 8 34)


;; Sequence Operations ;;
(print (map square (list 1 2 3 4 5)))
; (1 4 9 16 25)

;; filter ;;
(define (filter predicate sequence)
	(cond 
		((null? sequence)
			())
		((predicate (car sequence))
			(cons 
				(car sequence)
				(filter predicate (cdr sequence))))
		(else 
			(filter predicate (cdr sequence)))))
; test
(print (filter odd? (list 1 2 3 4 5))) ; (1 3 5)

;; accumulate ;;
(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op 
			(car sequence)
			(accumulate op initial (cdr sequence)))))

; test
(print (accumulate + 0 (list 1 2 3 4 5))) ; 15
(print (accumulate * 1 (list 1 2 3 4 5))) ; 120
(print (accumulate cons () (list 1 2 3 4 5))) ; (1 2 3 4 5)

;; sub ;;
(define (enumerate-interval low high)
	(if (> low high)
		()
		(cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
	(cond 
		((null? tree) 
			())
	((not (pair? tree)) 
		(list tree))
	(else 
		(append 
			(enumerate-tree (car tree))
			(enumerate-tree (cdr tree))))))
; test
(print (enumerate-interval 2 7)) ; (2 3 4 5 6 7)
(print (enumerate-tree (list 1 (list 2 (list 3 4)) 5))) ; (1 2 3 4 5)

;; using abstruct, these 2 procedures are shown such as this ;;
(define (sum-odd-squares tree)
	(accumulate 
		+
		0
		(map square
			(filter 
				odd?
				(enumerate-tree tree)))))
; test
(print (sum-odd-squares (list 1 2 3 4 5))) ; 35

(define (even-fibs n)
	(accumulate 
		cons
		()
		(filter even?
			(map fib
			(enumerate-interval 0 n)))))
; test
(print (even-fibs 10)) ; (2 8 34)

;; and more ;;
(define (list-fib-squares n)
	(accumulate 
		cons
		()
		(map square
			(map fib
			(enumerate-interval 0 n)))))
; test
(print (list-fib-squares 10)) ; (1 1 1 4 9 25 64 169 441 1156 3025)

(define (product-of-squares-of-odd-elements sequence)
	(accumulate 
		*
		1
		(map square
		(filter odd? sequence))))
(print (product-of-squares-of-odd-elements (list 1 2 3 4 5))) ; 255

(define (salary-of-highest-paid-programmer records)
	(accumulate 
		max
		0
		(map salary
		(filter programmer? records))))


;; Nested Mappings ;;
#|
 i | 2 3 4 4 5 6 6 
 j | 1 2 1 3 2 1 5
i+j| 3 5 5 7 7 7 11
|#

(lambda (n)
	(accumulate
		append
		()
		(map
			(lambda (i)
				(map 
					(lambda (j) 
						(list i j))
				(enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n))))

(define (flatmap proc seq)
	(accumulate append () (map proc seq)))

(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
	(map 
		make-pair-sum
		(filter prime-sum?
			(flatmap
				(lambda (i)
					(map 
						(lambda (j) 
							(list i j))
					(enumerate-interval 1 (- i 1))))
				(enumerate-interval 1 n)))))
; test
(print (prime-sum-pairs 6))
; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

(define (permutations s)
	(if (null? s) ; empty set?
		(list ()) ; sequence containing empty set
		(flatmap 
			(lambda (x)
				(map 
					(lambda (p) 
						(cons x p))
				(permutations (remove x s))))
			s)))

(define (remove item sequence)
	(filter 
		(lambda (x) 
			(not (= x item)))
		sequence))

; END