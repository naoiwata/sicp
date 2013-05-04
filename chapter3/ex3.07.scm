;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.7.
;;

(define (make-account balance password)
  (define (widthdraw amount)
    (if (>= balance amount)
      (begin 
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond
          ((eq? m 'widthdraw) widthdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MEKE-AMOUNT" m)))
        (error "Incorrect password" p)))
  dispatch)

(define (make-joint account based-password new-password)
  (define (dispatch password m)
    (if (eq? password new-password)
        (account based-password m)
        (error "Incorrect password -- PASSWORD" m)))
  dispatch)

; test
(define acc1 (make-account 100 'panda))
(print ((acc1 'panda 'widthdraw) 10)) ; 90
(define acc2 (make-joint acc1 'cat 'alpaca))
;(print ((acc2 'alpaca 'widthdraw) 10)) ; Incorrect password cat
(define acc3 (make-joint acc1 'panda 'alpaca))
(print ((acc3 'alpaca 'widthdraw) 10)) ; 80
(print ((acc1 'panda 'widthdraw) 10)) ; 70

; END