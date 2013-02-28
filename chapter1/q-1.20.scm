;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.20
;;

;(add-load-path "." :relative)
;(load "lib.scm")

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

(print (gcd 28 16))
; 4

; in normal order evaluation
(gcd 206 40)

(if (= 40 0) ; #f, (= 40 0)
		206
		(gcd 40 (remainder 206 40)))

(if (= (remainder 206 40) 0) ; #f, (= 6 0)
		40
		(gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(if (= (remainder 40 (remainder 206 40)) 0) ; #f, (= 4 0)
		(remainder 206 40)
		(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ; #f, (= 2 0)
		(remainder 40 (remainder 206 40))
		(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))) ; -> 11 counts

(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ; #t, (= 0 0) ; -> 7 counts
		(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
		(gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

; the number of remainder -> 7 + 11 -> 18 counts

; in applicative order evaluation
(gcd 206 40)

(if (= 40 0) ; #f
		206
		(gcd 40 (remainder 206 40)))

(if (= 6 0) ; #f
		40
		(gcd 6 (remainder 40 6)))

(if (= 4 0) ; #f
		6
		(gcd 4 (remainder 6 4)))

(if (= 2 0) ; #f
		4
		(gcd 2 (remainder 4 2)))

(if (= 0 0) ; #t
		2
		(gcd 0 (remainder 2 0
; the number of remainder -> 4 counts

; ref : http://naoiwata.blogspot.jp/2013/02/sicp-125-greatest-common-divisors.html
; END