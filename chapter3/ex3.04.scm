;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.4.
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
  (define (call-the-cops) (error "called 110 :("))
  (define incorrect-types-num 0)
  (define (dispatch p m)
    (if (eq? p password)
        (begin
          (set! incorrect-types-num 0)
          (cond
            ((eq? m 'widthdraw) widthdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MEKE-AMOUNT" m))))
        (begin
          (if (> incorrect-types-num 6)
              (call-the-cops)
              (begin
                (set! incorrect-types-num (+ incorrect-types-num 1))
                (error "Incorrect password"))))))
  dispatch)
  
; test
(define acc (make-account 100 'panda))
((acc 'panda 'widthdraw) 40)) ; 60
((acc 'alpaca 'widthdraw) 40)
((acc 'alpaca 'widthdraw) 40)
((acc 'alpaca 'widthdraw) 40)
((acc 'alpaca 'widthdraw) 40)
((acc 'alpaca 'widthdraw) 40)
((acc 'alpaca 'widthdraw) 40)
((acc 'alpaca 'widthdraw) 40) ; called 110 :(

; END
