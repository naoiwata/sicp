;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.12.
;;

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(print z) ; (a b c d)
(print (cdr x)) ; (b)

(define W (append! x y))

(print W) ; (a b c d)
(print (cdr x)) ; (b c d)

; END