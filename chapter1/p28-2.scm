;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.28 prime
;;

(add-load-path "." :relative)
(load "lib.scm")

(use srfi-27)

; Fermat test
(define (expmod a n m)
	(cond
		((= n 0) 1)
		((even? n)
			(remainder
				(square (expmod a (/ n 2) m))
				m))
		(else
			(remainder
				(* a (expmod a (- n 1) m))
				m))))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
	(cond
		((= times 0)
			#t)
		((fermat-test n)
			(fast-prime? n (- times 1)))
		(else
			#f)))

; END