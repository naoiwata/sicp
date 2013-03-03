;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.33
;;

(add-load-path "." :relative)
(load "lib.scm")
(load "q-1.28.scm")

; linear recursive procedure
(define (filtered-accumulate filter combiner null-value term a next b)
	(if (> a b)
		null-value
		(if (filter a)
			(combiner 
				(term a)
				(filtered-accumulate filter combiner null-value term (next a) next b))
			(filtered-accumulate filter combiner null-value term (next a) next b))))



; iteative procedure
(define (filtered-accumulate-i filter combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(if (filter a)
				(iter (next a) (combiner (term a) result))
				(iter (next a) result))))
	(iter a null-value))

; (a)
(define (prime? n)
	(fast-prime? n 2))

(define (sum-prime a b)
	(filtered-accumulate prime? + 0 square a inc b))

(define (sum-prime-i a b)
	(filtered-accumulate-i prime? + 0 square a inc b))

; test
(sum-prime 2 10)
; -> 384
(sum-prime-i 2 10)
; -> 384

; (b)
(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

(define (sum-prime-int n)
	(define (prime-int? i)
		(= (gcd i n) 1))
	(define (this k) k)
	(filtered-accumulate prime-int? * 1 this 1 inc (- n 1)))
	
; test
(sum-prime-int 10)
; -> 189

; END