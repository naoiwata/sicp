;;
;; @author naoiwata
;; SICP Chapter3
;; 3.1.1 Local State Variables
;;

; (widthdraw 25) ; -> 75
; (widthdraw 25) ; -> 50
; (widthdraw 60) ; -> "Insufficient funds"
; (widthdraw 15) ; -> 35

; ---------------------------------------------------------
(define balance 100)

(define (widthdraw amount)
  (if (>= balance amount)
      (begin 
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))

; ---------------------------------------------------------

(define new-withdraw
  (let
    ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))))

; ---------------------------------------------------------

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin 
        (set! balance (- balance amount))
        balance)
      "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(print (W1 50)) ; 50
(print (W2 70)) ; 30
(print (W2 40)) ; Insufficient funds
(print (W1 40)) ; 10

; ---------------------------------------------------------

(define (make-account balance)
  (define (widthdraw amount)
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
      ((eq? m 'widthdraw) widthdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request -- MEKE-AMOUNT" m))))
  dispatch)

(define acc (make-account 100))

(print ((acc 'widthdraw) 50)) ; 50
(print ((acc 'widthdraw) 60)) ; Insufficient funds
(print ((acc 'deposit)   40)) ; 90
(print ((acc 'widthdraw) 60)) ; 30

(define acc2 (make-account 100))

; END