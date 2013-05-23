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

; another solution

(define (make-accumlator2 initial-value)
  (lambda (value)
      (set! initial-value (+ initial-value value))
      initial-value))

(define B (make-accumlator2 5))

(print (B 10)) ; 15
(print (B 10)) ; 25

; END