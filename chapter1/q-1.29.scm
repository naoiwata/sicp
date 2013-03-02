;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.29
;;

(add-load-path "." :relative)
(load "p32.scm")

(define (integral-h f a b n)
	(define h 
		(/ (- b a) n))
	(define (g k)
		(f (+ a (* k h))))
	(define (inc i)
		(+ i 1))
	(define (term k)
		(cond
			((or (= k 0) (= k n))
				(g k))
			((even? k)
				(* 2 (g k)))
			(else (* 4 (g k)))))
	(*
		(/ h 3)
		(sum term 0 inc n)))

; test
(print
	; n = 100
	(integral cube 0 1 100) ; 3.139592655589783
	(exact->inexact (integral-h cube 0 1 100)) ; 00.25
	
	; n = 1000
	(integral cube 0 1 1000) ; 0.249999875000001
	(exact->inexact (integral-h cube 0 1 1000))) ; 00.25

; as this results show that simpson's method is more high precision.

; END