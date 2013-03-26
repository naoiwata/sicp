;; @author naoiwata
;; SICP Chapter2
;; question 2.62

(add-load-path "." :relative)
(load "q2.61.scm")

(define (union-set set1 set2)
  (cond
    ((null? set1)
     set2)
    ((null? set2)
     set1)
    (else
      (let
        ((x1 (car set1)) (x2 (car set2)))
        (cond
          ((= x1 x2)
           (cons x1 (union-set (cdr set1) (cdr set2))))
          ((< x1 x2)
           (cons x1 (union-set (cdr set1) set2)))
          ((< x2 x1)
           (cons x2 (union-set set1 (cdr set2)))))))))
; test
(print (union-set (list 1 2 3 4) (list 3 4 5))) ; (1 2 3 4 5)

; END