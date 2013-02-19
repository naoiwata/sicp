;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.16
;;

(add-load-path "." :relative)
(load "lib.scm")

(define (fast-expt b n)
	(fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
	(cond
		((= n 0)
			a)
		((even? n)
			(fast-expt-iter
				a
				(* b b)
				(/ n 2)))
		(else
			(fast-expt-iter
				(* a b)
				b
				(- n 1)))))

; test
(print (fast-expt 2 8))
; 256

;; END