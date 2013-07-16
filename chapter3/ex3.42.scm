;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.42.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let 
    ((protected (make-serializer)))
    (let 
      ((protected-withdraw (protected withdraw))
       (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond 
          ((eq? m 'withdraw) protected-withdraw)
          ((eq? m 'deposit) protected-deposit)
          ((eq? m 'balance) balance)
          (else 
            (error "Unknown request -- MAKE-ACCOUNT" m))))
      dispatch)))

; ------------------------------------------------------------------------
; discussion
; ------------------------------------------------------------------------

; 安全な変更であり, 並列性に相違は無い.
; 環境モデルの図を描くと分かりやすいが, 手続きは違えど局所変数の指すポインタと環境は同じだからである.

; END