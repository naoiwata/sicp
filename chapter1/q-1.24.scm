;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.24
;;

(define (square n)
	(* n n))

(define (expmod a n m)
	(cond
		((= n 0) 1)
		((even? n)
			(remainder
				(square (expmod a (/ n 2) m))
				m))
		(else
			(remainder
				(* a (expmod a (- n 1) m))
				m))))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond
		((= times 0)
			#t)
		((fermat-test n)
			(fast-prime? n (- times 1)))
		(else
			#f)))

(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if (fast-prime? n 2)
		(report-prime 
			(- (runtime) start-time))))

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time))

(define (search-for-primes x y)
	(cond
		((even? x)
					(search-for-primes (+ x 1) y))
		((< x y)
				(timed-prime-test x)
				(search-for-primes (+ x 2) y))))

; test
; on more large number
(search-for-primes    1000    1040)
;    1009 *** 26
(search-for-primes   10000   10040)
;   10007 *** 38
(search-for-primes  100000  100040)
;  100003 *** 51
(search-for-primes 1000000 1000040)
; 1000003 *** 73

;   n    | 10^3 | 10^4 | 10^5 | 10^6 |
; result |  26  |  38  |  51  |  73  |
; log(n) | 6.90 | 9.21 | 11.51| 13.81|
; they increased as log(n),
; so the hypothesis and the result are matched:)
; -> このアルゴリズムでは、nに対する増加の速度はΘ(log(n)だと推測される。
; -> 実験結果は、nに対して(log(n))倍ずつ増加しているので仮説が正しいことが確認された。

; END