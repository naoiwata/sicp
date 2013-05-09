;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.13.
;;

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c))) ; endless loop

; END