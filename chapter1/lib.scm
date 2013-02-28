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
(define (square n)
	(* n n))

;; runtime
;; thanks to http://sicp.g.hatena.ne.jp/n-oohira/20090122/1232632508
;; return micro-sec
(use srfi-11)
(define (runtime)
	(let-values
		(((a b) (sys-gettimeofday)))
			(+ (* a 1000000) b)))

;; END