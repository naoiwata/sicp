;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.45.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let 
    ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond 
        ((eq? m 'withdraw)   (balance-serializer withdraw))
        ((eq? m 'deposit)    (balance-serializer deposit))
        ((eq? m 'balance)    balance)
        ((eq? m 'serializer) balance-serializer)
        (else 
          (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (deposit account amount)
  ((account 'deposit) amount))

; ------------------------------------------------------------------------
; discussion
; ------------------------------------------------------------------------

; Louis の定義した手続きでは, 定義した account と amount に対する balance-serializer 手続きの
; 参照するポインタが異なる, 即ち別環境のものとして処理されるので直列化として処理されないと考えられる.
; この為 serialized-exchange 手続きを呼び出した時, exchange 手続きの ((account1 'withdraw) difference)
; に注目すると, この (account1 'withdraw) 手続きは (balance-serializer withdraw) 手続きを呼ぶ.
; ((account1 'withdraw) difference) 手続きは balance-serializer として定義され先に直列化されているので, 
; 中の, この後に直列化された (balance-serializer withdraw) 手続きを先に評価することができず, 
; この serialized-exchange 手続きを評価することができなくなる.

; END