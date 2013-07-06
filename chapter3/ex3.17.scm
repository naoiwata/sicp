;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.17.
;;

(define count-pairs
  (let 
    ((counted '()))  
    (lambda (x)
      (if (or (not (pair? x))
              (memq x counted))
          0
          (begin
            (set! counted (cons x counted))
            (+ (count-pairs (car x))
               (count-pairs (cdr x))
               1))))))

(define W1 '(a b c))
(print W1) ; (a b c)
(print (count-pairs W1)) ; 3

(define W2-1 '(a))
(define W2 (list W2-1 W2-1))
(print W2) ; ((a) (a))
(print (count-pairs W2)) ; 3

(define W3-1 '(a))
(define W3-2 (cons W3-1 W3-1))
(define W3 (cons W3-2 W3-2))
(print W3) ; (((a) a) (a) a)
(print (count-pairs W3)) ; 3

; END
