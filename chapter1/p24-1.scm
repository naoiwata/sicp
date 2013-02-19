;;
;; @author naoiwata
;; SICP Chapter1
;; p24 Fibnacci
;;

;; linear recursive
(define (Fib-l n)
	(cond
		((= n 0) 0)
		((= n 1) 1)
		(else (+ (Fib-l (- n 1)) (Fib-l (- n 2))))))

(print (Fib-l 3))
; 8

;; itearive
(define (Fib n)
	(Fib-iter 0 1 n))

(define (Fib-iter sum pre count)
	(if (= count 0)
		sum
		(Fib-iter
			(+ sum pre)
			sum
			(- count 1))))

;; END