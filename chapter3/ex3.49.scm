;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.49.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

; その環境の中で共通資源Sがあり, その系のプロセスA, B(id:A < id:B)の処理にはSにアクセスすることが必須としよう.
; 先に実行されるプロセスAによってSの値が書き換えられ, その値がある条件を満たすと別のプロセスCが走ることになり, 
; かつこのプロセスCが別の共通資源Qにアクセスする処理になっていた場合を考える.
; このとき, もしQを必要とする別のプロセスDが走りQを獲得している最中だった場合, このQへの処理がプロセスCとDによって
; 同時に行われてしまいこの系全体の相互排除器としての機能は破綻する.
; つまり, 共通資源を扱うその環境内で全てのプロセス(その共通資源を扱う可能性のあるプロセス全て)において, 
; どの順番でプロセスが実行されようと共通資源への操作(代入)の許可, 非許可, 並びにデッドロックが行った場合の回復処理を
; 一元管理できるシステムとしてプログラムを構築しなければならないと考えられる.

; END