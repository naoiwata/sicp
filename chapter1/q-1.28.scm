;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.28
;;

; carmichael numbers are 561, 1105, 1729, 2465, 2821, 6601, 8911...
(define (square n)
	(* n n))

(define (expmod-mr a n m)
	(cond 
		((= n 0) 
			1)
		((even? n)
			(test a n m))
		(else
			(remainder 
				(* a (expmod-mr a (- n 1) m))
				m))))

(define (test a n m)
	(if (mr? (expmod-mr a (/ n 2) m) m)
		(remainder
			(square (expmod-mr a (/ n 2) m))
			m)
		0))

(define (mr? p n)
	(if
		(and
			(not (= p 1))
			(not (= p (- n 1)))
			(= (remainder (square p) n) 1))
		#f
		#t))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod-mr a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond
		((= times 0)
			#t)
		((fermat-test n)
			(fast-prime? n (- times 1)))
		(else
			#f)))

; test
(fast-prime? 561 2)
; -> #f
(fast-prime? 1105 2)
; -> #f
(fast-prime? 1729 2)
; -> #f
(fast-prime? 2465 2)
; -> #f
(fast-prime? 2821 2)
; -> #f
(fast-prime? 29 2)
; -> #t

; END