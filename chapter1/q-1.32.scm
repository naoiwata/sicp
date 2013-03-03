;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.32
;;

(add-load-path "." :relative)
(load "lib.scm")

; (a) linear recursive procedure
; (accumulate combliner null-value term a next b)
(define (accumulate combliner null-value term a next b)
	(if (> a b)
		null-value
		(combliner 
			(term a)
			(accumulate combliner null-value term (next a) next b))))

; test 

(define (sum n)
	(accumulate + 0 square 1 inc n))
	
(print
	(sum 10))
; -> 385

; (b) iteative procedure
(define (accumulate-i combliner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combliner (term a) result))))
	(iter a null-value))

; test
(define (sum-i n)
	(accumulate-i + 0 square 1 inc n))
	
(print
	(sum-i 10))
; -> 385

; END