;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.16.
;;

; Ben
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; test
(print (count-pairs '(1 2 3))) ; 3
(print (count-pairs '(a b))) ; 2
(print (count-pairs (cons 'a 'b))) ; 1
(print (count-pairs '(a (b c)))) ; 4
(print (count-pairs '(cons (cons (a b)) c))) ; 7

; END
