;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.48.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (make-account-and-serializer balance id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
          balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (let
    ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond
        ((eq? m 'withdraw)   withdraw)
        ((eq? m 'deposit)    deposit)
        ((eq? m 'balance)    balance)
        ((eq? m 'serializer) balance-serializer)
        ((eq? m 'id) id)
        (else 
          (error "unknown request" m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let
    ((serializer1 (account1 'serializer))
     (serializer2 (account2 'serializer)))
    (if (< (id account1) (id account2))
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account2
         account1))))

; END