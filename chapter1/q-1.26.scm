;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.26
;;

; question copy - (1)
(define (expmod-q a n m)
	(cond 
		((= n 0) 
			1)
		((even? n)
			(remainder 
				(* (expmod-q a (/ n 2) m)   ; <-
					(expmod-q a (/ n 2) m)) ; <-
				m))
		(else
			(remainder 
				(* a (expmod-q a (- n 1) m))
				m))))

; q1-24 solution - (2)
(define (expmod a n m)
	(cond
		((= n 0) 
			1)
		((even? n)
			(remainder
				(square (expmod a (/ n 2) m)) ; <-
				m))
		(else
			(remainder
				(* a (expmod a (- n 1) m))
				m))))

; look at <- procedure
; (1) is need to eval arguments of expmod-q on 2 times.
; but (2) is one time.
; (2)では余りを求める分子の(expmod a (/ n 2) m)の2乗を評価する際に、square手続きに1回この引数を評価すれば良い。
; しかし、(1)では余りを求める分子の(expmod-q a (/ n 2) m)の2乗を評価する際に、expmod-qの中の引数をそれぞれ2回評価しなければならない。
; 評価する回数分だけ、その手続きの処理速度は遅くなる。
; 例えばnが8の場合、(2)では、手続きのステップ数は
; N(b^8) 
; -> N(b^4*b^4)
; -> N(f(b^4)*f(b^4))
; -> N(f(b^2)*f(b^2)*f(b^2)*f(b^2))
; と、3(log8)回の処理になるが、(1)では、手続きのステップ数は
; N(b^8)
; -> N(f(b^8)) 
; -> N(f(N(4)*N(4)))
; -> N(f(f(b^4*b^4)))
; -> N(f(f(N(2)*N(2)*N(2)*N(2))))
; -> N(f(f(f(2)*f(2)*f(2)*f(2))))
; -> N(f(f(f(b^2)*f(b^2)*f(b^2)*f(b^2))))
; -> N(f(f(b^2)*f(b^2)*f(b^2)*f(b^2)))
; -> N(f(b^2)*f(b^2)*f(b^2)*f(b^2))
; と、8回の処理になる。
; 参考：http://naoiwata.blogspot.jp/2013/02/sicp-successive-squaring.html
; よって、(2)の手続きでは増加の程度がΘ(log(n))であるのに対し、アルゴリズムを書き換えた(1)ではΘ(n)になってしまう。

; END