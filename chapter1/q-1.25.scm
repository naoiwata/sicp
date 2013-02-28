;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.25
;;

(define (square n)
	(* n n))

; changed from here
(define (expmod-q base exp m)
	(remainder (fast-expt base exp) m))

(define (fast-expt base exp)
	(cond
		((= exp 0) 
			1)
		((even? exp)
			(square (fast-expt base (/ exp 2))))
		(else
			(* base (fast-expt base (- exp 1))))))
; end

(define (fermat-test n)
	(define (try-it a)
		(= (expmod-q a n n) a))
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
; in pre question high speed procedure
(search-for-primes 1000000 1000040)
; 1000003 *** 89

; in this question's procedure
(search-for-primes 1000000 1000040)
; 1000003 *** 8559874

; in this question's algorithm is sooooo slow X(
; nが1000000付近の整数の時、本問題の手続きは前回の高速手続きに比べて約10万倍も処理速度が遅いことが分かった。
; 高速手続きと本手法の違いは、扱う引数の値の大きさである。
; 高速手続きでは割り算の余りの値を再帰的に引数に返して処理をしているが、本手法では2乗した大きな値を引数に返す。
; この為、本手法ではfast-exptで評価する値が大きくなり、余りを求めるremainderの処理により多くの時間がかかってしまうと考えられる。

; END