
;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.14.
;;

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let
          ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define W '(a b c d))
(print W) ; (a b c d)
(print (mystery '(a b c d))) ; (d c b a)

; EMD