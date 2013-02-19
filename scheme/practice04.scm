;
; @author naoiwata
; scheme
; http://www.shido.info/lisp/scheme4.html
;

; (1) 次の関数を書いてください。簡単な関数ですが、しばしば使うことになると思います。
;     これらの関数は R6RS では定義されていませんが、便利なので、 この解説記事では今後説明抜きで使います。

; 1. 引数に 1 を加えて返す関数 (inc)
(define (inc n)
	(+ n 1))

; 2. 引数から 1 を引いて返す関数 (dec)
(define (dec n)
	(- n 1))

; (2) ボールを投げたときに飛ぶ距離を求める関数を以下の手順で書いてみようと思います。

; 1. 初速度 v で角度 theta 度で投げたものが飛ぶ距離を求める関数
(define pi
	(* 4 (atan 1.0)))

(define (radian n)
	(* n (/ pi 180)))

; V = v - g * (t / 2) = 0
; L = v * cosx * t 
;   = v * cosx * * 2 * v * sinx / g
;   = v^2 * sinx * cosx / g
; g = 9.8
(define (dx v n)
	(/ (* 2 v v (sin (radian n)) (cos (radian n))) 9.8))

; 2. 初速度 40 m s-1, 角度 30 度で投げたボールが飛ぶ距離を上で定義した関数を用いて求めよ。
; (dx 40 30)
; 141.39190265868385