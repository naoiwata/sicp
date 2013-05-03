;;
;; @author naoiwata
;; SICP Chapter3
;; 3.1.3 The Costs of Introducing Assignment
;;

; make-simlified-withdraw -----------------------------
(define (make-simlified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simlified-withdraw 25))

(print (W 20)) ; 5
(print (W 10)) ; -5

; make-decrementer ------------------------------------
(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))

(print (D 20)) ; 5
(print (D 10)) ; 15

; compare ---------------------------------------------
(print ((make-decrementer 25) 20)) ; 5
(print ((make-simlified-withdraw 25) 20)) ; 5

;; Sameness and change ;;
(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))

(print (D1 20)) ; 5
(print (D1 20)) ; 5
(print (D2 20)) ; 5

(define W1 (make-simlified-withdraw 25))
(define W2 (make-simlified-withdraw 25))

(print (W1 20)) ; 5
(print (W1 20)) ; -15
(print (W2 20)) ; 5

;; Pitfalls of imperative programming ;;

; functional programing -------------------------------
(define (fanctional n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(print (fanctional 10)) ; 3628800

; imperative programing -------------------------------
(define (fanctional n)
  (let
    ((product 1)
     (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin
            (set! product (* counter product))
            (set! counter (+ counter 1))
            (iter))))
    (iter)))

(print (fanctional 10)) ; 3628800

(define (fanctional-error n)
  (let
    ((product 1)
     (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin
            (set! counter (+ counter 1))
            (set! product (* counter product))
            (iter))))
    (iter)))

(print (fanctional-error 10)) ; 39916800

; END