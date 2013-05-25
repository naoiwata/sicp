;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.17.
;;

(define count-pairs
  (let
    ((counted '()))
    (lambda (x)
      (cond
        ((not (pair? x)) 0)
        ((memq x counted) 0)
        (else
          (set! counted (cons x counted))
          (+ (count-pairs (car x))
             (count-pairs (cdr x))
             1))))))

; test
(define list3 '(a b c))
 
(define list4-1 '(b c))
(define list4-2 '(a))
(set-car! list4-1 list4-2) ; ((a) c)
(set-car! (cdr list4-1) list4-2) ; ((a) (a))
 
(define list7-1 '(c))
(define list7-2 '(b))
(define list7-3 '(a))
(set-car! list7-2 list7-3) ; ((a))
(set-cdr! list7-2 list7-3) ; ((a) a)
(set-car! list7-1 list7-2) ; (((a) a))
(set-cdr! list7-1 list7-2) ; (((a) a) (a) a)
 
(define linf '(a b c))
(set-cdr! (cdr (cdr linf)) linf)
 
(print (count-pairs list3))   ; 3
(print (count-pairs list4-1)) ; 3
(print (count-pairs list7-1)) ; 3
(print (count-pairs linf))    ; 3

; END