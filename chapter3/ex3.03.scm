;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.3.
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

; test
(define acc (make-account 100 'panda))
(print ((acc 'panda 'widthdraw) 40)) ; 40
(print ((acc 'alpaca 'widthdraw) 40)) ; Incorrect password alpaca

; END