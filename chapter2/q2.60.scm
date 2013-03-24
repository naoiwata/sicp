;; @author naoiwata
;; SICP Chapter2
;; question 2.60

(add-load-path "." :relative)
(load "q2.59.scm")

(define (adjoin-set x set)
	(cons x set))

(define (union-set set1 set2)
	(append set1 set2))
; test 
(print (adjoin-set 1 set1)) ; (1 1 2 3 4 5)
(print (union-set set1 set2)) ; (1 2 3 4 5 2 4 6 8 10)

#|
(1) 重複無し表現の対応する手続きと比べて効率はどうなるか
上に示すように重複の表現を許すにはadjoin-set手続きとunion-set手続きのみ改変すればよい。
重複を許すことで、adjoin-set手続きは条件分岐が無くなった分、ステップ数はΘ(n)からΘ(1)へと変わる。
また、その他のelement-of-set?、element-of-set?、element-of-set?手続きにおいては、
これらにadjoin-set手続きは含まれていない為、adjoin-set手続きの改変による効率の変化には影響がないと考えられる。
同様に、union-set手続きも条件分岐が無くなった分、ステップ数はΘ(n)からΘ(1)へと変わる。
よって、重複を許すことでadjoin-set、union-set手続きはステップ数がΘ(n)からΘ(1)へと減少する。

(2) 重複無し表現よりこの方法の方が使いたくなる応用はあるだろうか
重複表現を許すことで評価するsetリストの長さが増えてしまうが、
adjoin-set、union-setの仕様回数が多い手続きを実装する場合にはこの方法が有用だと考えられる。
|#

; END