;; @author naoiwata
;; SICP Chapter2
;; question 2.61

(add-load-path "." :relative)
(load "q2.60.scm")

(define (adjoin-set x set)
  (cond
    ((null? set)
     (list x))
    ((= x (car set))
     set)
    ((< x (car set))
     (cons x set))
    ((< (car set) x)
     (cons (car set) (adjoin-set x (cdr set))))))
; test
(print (adjoin-set 1 set1)) ; (1 2 3 4 5)
(print (adjoin-set 3 set1)) ; (1 2 3 4 5)
(print (adjoin-set 5 set1)) ; (1 2 3 4 5)

; END