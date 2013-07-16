;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.41.
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
    (define (dispatch m)
      (cond 
        ((eq? m 'withdraw) 
         (protected withdraw))
        ((eq? m 'deposit) 
         (protected deposit))
        ((eq? m 'balance)
         ((protected (lambda () balance)))) ; serialized
        (else 
          (error "Unknown request -- MAKE-ACCOUNT"
                 m))))
    dispatch))

; ------------------------------------------------------------------------
; discussion
; ------------------------------------------------------------------------

; 直列化されたwithdrawやdepositは共に代入を伴う手続きであるが, 直列化でなくとも問題無い.
; これらはset!の代入とbalanceを返す手続きからできているだけので, たとえ直列化をしなくともbalanceには
; set!によって値が代入され変化しbalanceを返さないだけで, 読み取りには問題無いからである.

; END