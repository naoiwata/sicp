;;
;; @author naoiwata
;; SICP Chapter3
;; 3.2.2 Applying Simple Procedures
;;	

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(print (f 5)) ; 136

