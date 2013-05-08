;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.11.
;;

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else
        (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 50))

(print ((acc 'deposit) 40)) ; 90
(print ((acc 'withdraw) 60)) ; 30

(define acc2 (make-account 100))

; END