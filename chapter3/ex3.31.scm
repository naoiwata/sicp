;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.31.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (accept-action-procedure! proc)
  (set! action-procedures (cons proc action-procedures))
  (proc))

(define (accept-action-procedure! proc)
  (set! action-procedures (cons proc action-procedures)))

; wireに手続きを追加した後にその手続きを走らせておくことで, (make-agenda)が空の状態になり手続きが評価されないのを防ぎ, 
; wireに手続きを追加したすぐ後にこの手続きが駆動されても評価されるようにしている.

; END