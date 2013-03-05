;;
;; @author naoiwata
;; lib.scm
;;

;; Fibnacci
(define (Fib n)
	(Fib-iter 0 1 n))

(define (Fib-iter sum pre count)
	(if (= count 0)
		sum
		(Fib-iter
			(+ sum pre)
			sum
			(- count 1))))

;; lib 
(define (inc n)
	(+ n 1))

(define (dec n)
	(- n 1))

(define (square n)
	(* n n))

(define (average x y)
	(/
		(+ x y)
		2))

(define (abs n)
	(if (< n 0)
		(- 0 n)
		n))

(define (positive? n)
	(if (< 0 n)
		#t
		#f))

(define (negative? n)
	(if (< n 0)
		#t
		#f))

;; runtime
;; thanks to http://sicp.g.hatena.ne.jp/n-oohira/20090122/1232632508
;; return micro-sec
(use srfi-11)
(define (runtime)
	(let-values
		(((a b) (sys-gettimeofday)))
			(+ (* a 1000000) b)))

;; END