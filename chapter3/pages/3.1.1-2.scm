;;
;; @author naoiwata
;; SICP Chapter3
;; 3.1.1 Local State Variables
;;

(define (make-withdraw balance)
  (lambda (amount)
    (if (<= amount balance)
        (begin (set! balance (- balance amount))
                balance)
        "error")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(print (W1 10)) ; 90
(print (W2 40)) ; 60
(print (W1 50)) ; 40
(print (W2 100)) ; error