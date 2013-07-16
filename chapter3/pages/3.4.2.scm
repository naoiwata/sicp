;;
;; @author naoiwata
;; SICP Chapter3
;; 3.4.2 Mechanisms for Controlling Concurrency
;;

; ------------------------------------------------------------------------
; Serializing access to shared state
; ------------------------------------------------------------------------

; ------------------------------------------------------------------------
; Serializers in Scheme
; ------------------------------------------------------------------------

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda (x) (set! x (* x x)))
                  (lambda (x) (set! x (* x 1))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let
    ((protected (make-serializer)))
    (define (dispatch m)
      (cond
        ((eq? m 'withdraw) (protected withdraw))
        ((eq? m 'deposit)  (protected deposit))
        ((eq? m 'balance)  (protected balance))
        (else 
          (error "unknown request" m))))))

