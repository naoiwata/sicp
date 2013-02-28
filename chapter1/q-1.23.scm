;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.23
;;

(define (square n)
	(* n n))

(add-load-path "." :relative)
(load "lib.scm")
(load "q-1.22.scm")

(define (smallest-divisor n)
	(find-divisor n 2))

(define (find-divisor n a)
	(cond
		((< n (square a)) n)
		((divisors? n a) a)
		(else (find-divisor n (next a))))) ;(+ a 1) -> (next a)

(define (divisors? n a)
	(= (remainder n a) 0))

(define (prime? n)
	(= n (smallest-divisor n)))

; solution
(define (next a)
	(if (= a 2)
		3
		(+ a 2)))


(define (even? n)
	(= (remainder n 2) 0))

; test
(search-for-primes 1000 1030)
; -> time average: 14 : t1'
; 1009 *** 14
; 1013 *** 14
; 1019 *** 14

(search-for-primes 10000 10040)
; -> time average: 71 : t2'
; 10007 *** 73
; 10009 *** 73
; 10037 *** 67

(search-for-primes 100000 100050)
; -> time average: 220.33 : t3'
; 100003 *** 233
; 100019 *** 114
; 100043 *** 114

(search-for-primes 1000000 1000040)
; -> 1000003, 1000033, 1000037
; -> time average: 360.67 : t4'
; 1000003 *** 361
; 1000033 *** 363
; 1000037 *** 358

; discussion
; t1 / t1' = 21.67 / 14 = 1.55
; t2 / t2' = 68.33 / 71 = 0.96
; t3 / t3' = 333.67 / 220.22 = 1.52
; t4 / t4' = 1124.67 / 360.67 = 3.11
; this new program spped is shorter than before one, 
; but ratio is not always twise... :( 
; then, oncreased the number, this was go well:)

; END