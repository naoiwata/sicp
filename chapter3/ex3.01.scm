;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.1.
;;

(define (make-accumlator initial-value)
  (let
    ((sum initial-value))
    (lambda (value)
      (set! sum (+ sum value))
      sum)))

(define A (make-accumlator 5))

(print (A 10)) ; 15
(print (A 10)) ; 25

; END