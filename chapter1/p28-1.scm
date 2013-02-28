;;
;; @author naoiwata
;; SICP Chapter1
;; p28 prime
;;

(add-load-path "." :relative)
(load "lib.scm")

; Searching for Divisors Procedure
(define (smallest-divisor n)
	(find-divisor n 2))

(define (find-divisor n a)
	(cond
		((< n (square a)) n)
		((divisors? n a) a)
		(else (find-divisor n (+ a 1)))))

(define (divisors? n a)
	(= (remainder n a) 0))

(define (prime? n)
	(= n (smallest-divisor n)))

; test
(print
	(prime? 29))
; #t

; END