;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.10.
;;

(define (make-withdraw initial-amount)
  (let
    ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insuffient funds"))))

(define W1 (make-withdraw 100))
(print (W1 50)) ; 50
(define W2 (make-withdraw 100))
