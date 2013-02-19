;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.19
;;

;(add-load-path "." :relative)
;(load "lib.scm")

(define (Fib n)
	(Fib-iter 1 0 0 1 n))

(define (Fib-iter a b p q count)
	(cond
		((= count 0)
			b)
		((even? count)
			(Fib-iter
				a
				b
				(+
					(* p p)
					(* q q))
				(+
					(* 2 p q)
					(* q q))
				(/ count 2)))
		(else
			(Fib-iter
				(+
					(* b q)
					(* a q)
					(* a p))
				(+
					(* b p)
					(* a q))
				p
				q
				(- count 1)))))

; test
(print (Fib 7))
; 13

;; END