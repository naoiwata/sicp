;;
;; @author naoiwata
;; SICP Chapter3
;; 3.2.3 Frames as the Repository of Local State
;;	

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insuffient funds")))

(define W1 (make-withdraw 100))
(print (W1 50)) ; 50