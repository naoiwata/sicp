;; @author naoiwata
;; SICP Chapter2
;; question 2.43

; default flatmap
(flatmap
	(lambda (rest-of-queens)
		(map 
			(lambda (new-row)
				(adjoin-position new-row k rest-of-queens))
		(enumerate-interval 1 board-size)))
	(queen-cols (- k 1)))

; Louis's flatmap
(flatmap
	(lambda (new-row)
		(map 
			(lambda (rest-of-queens)
				(adjoin-position new-row k rest-of-queens))
		(queen-cols (- k 1))))
	(enumerate-interval 1 board-size))

#|
正規の手続きは、まずmap内は1〜board-size、即ちboard-size回の処理が実行され、
board-size個のリストが含まれるリストが生成される。(つまり、まず列を生成する)
その後、生成された各々のリスト要素に対して行に入ると想定される全ての場合分だけリストを生成する。
一方Louisの手続きでは、1個の列に対して行に入ると想定される全ての場合分のリストを生成し、
次に2個の列に対して同様の操作、次に3個、、、と、board-size個の列まで繰り返す。
よって、正規の手続きの処理時間をTとおくと、Louisの手続きの処理時間はT^(board-size)と考えられる。
|#

; END