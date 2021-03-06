;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.46.
;;

; ------------------------------------------------------------------------
; discussion
; ------------------------------------------------------------------------

; あるプロセスAが set-and-test! 手続きを走らせセルが偽であると見つけた瞬間から (set-car! cell #t) によって
; セルを真に代入するまでの間に, 別のプロセスBが set-and-test! 手続きを走らせてセルが偽であることを見つけてしまうと
; プロセスAとプロセスBが同時に相互排除器を獲得するのを許してしまい, 想定していた直列処理としての処理が進行しない.

; END