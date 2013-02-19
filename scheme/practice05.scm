;
; @author naoiwata
; scheme
; http://www.shido.info/lisp/scheme5.html
;

; (1) 次の関数を作ってください。

; 1. 実数の絶対値をを求める関数
(define (abs n)
	(if (< n 0)
		(- 0 n)
		n))
; (abs 100)
; (abs -100)

; 2. 実習の逆数を求める関数。引数が 0 のときは #f を返すようにしてください。
(define (inverce n)
	(if (= n 0)
		#f
		(/ 1 n)))

; 3. 整数を ASCII 文字のうち図形文字に変換する関数。 
; 図形文字に変換できる整数は 33 – 126 です。 
; 整数を文字に変換するには integer->char 関数を使います。 変換できないときは #f を返すようにして下さい。
(define (fact n)
	(if (and (<= 33 n) (<= n 126))
		(integer->char n))
	#f)
; (fact 122)
; #\z

; (2) 次の関数を作ってください。

; 1. 与えられた３つの実数が全て正ならその積を返す関数
(define (positive n)
	(if (<= 0 n)
		n
		#f))

(define (func1 a b c)
	(and (positive a)
		(positive b)
		(positive c)
	(* a b c)))

; 2. 与えられた３つのうちのどれか１つが負ならその積を返す関数。
(define (negative n)
	(if (< n 0)
		n
		#f))

(define (func2 a b c)
	(cond
		((negative a) (* a b c))
		((negative b) (* a b c))
		((negative c) (* a b c))
		(else #f)))

; (3) 次の関数を作ってください。

; 1. 試験の点数に応じて A--D の評価をつけます。点数を引数にとり、評価を返す関数を書いてください。
; 80 点以上 A
; 60 点以上 79 点以下 B
; 40 点以上 59 点以下 C
; 40 点未満 D
(define (score n)
	(cond
		((<= 80 n) 'A)
		((and (<= n 60) (<= 79 n)) 'B)
		((and (<= n 40) (<= 59 n)) 'C)
		((<= n 40) 'D)))

; END