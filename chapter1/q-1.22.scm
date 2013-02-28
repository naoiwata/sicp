;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.22
;;

;; p28 copy

(define (square n)
	(* n n))

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

;; question copy

(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if (prime? n)
		(report-prime 
			(- (runtime) start-time))))

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time))

;; solution 

(define (search-for-primes x y)
	(cond
		((even? x)
					(search-for-primes (+ x 1) y))
		((< x y)
				(timed-prime-test x)
				(search-for-primes (+ x 2) y))))

(define (even? n)
	(= (remainder n 2) 0))

; test

(search-for-primes 1000 1030)
; -> 1009, 1013, 1019
; -> time average: 21.67 : t1
; 1009 *** 24
; 1013 *** 21
; 1019 *** 20
; 1021 *** 21

(search-for-primes 10000 10040)
; -> 10007, 10009, 10037
; -> time average: 68.33 : t2
; 10007 *** 75
; 10009 *** 66
; 10037 *** 64
; 10039 *** 64

(search-for-primes 100000 100060)
; -> 100003, 1000019, 100047
; -> time average: 333.67 : t3
; 100003 *** 327
; 100019 *** 352
; 100043 *** 322
; 100049 *** 195
; 100057 *** 197

(search-for-primes 1000000 1000100)
; -> 1000003, 1000033, 1000037
; -> time average: 1124.67 : t4
; 1000003 *** 1157
; 1000033 *** 1111
; 1000037 *** 1106
; 1000039 *** 1018
; 1000081 *** 611
; 1000099 *** 609

; discussion
;       √10 = 3.16
; √t2 / √t1 = 1.77
; √t4 / √t3 = 1.83
; they does not match to √10... :(

; on more large number
(search-for-primes 10000000000000 10000000000038)
; 10000000000037 *** 1480044
(search-for-primes 1000000000000000 1000000000000050)
; 1000000000000037 *** 15972611
; -> about √10 times inceased!!!:)

; END