;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.18.
;;

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (cycle? x)
  (define visited '())
  (define (iter x)
    (set! visited (cons x visited))
    (cond
      ((null? (cdr x)) #f)
      ((memq (cdr x) visited) #t)
      (else
        (iter (cdr x)))))
  (iter x))

(print (cycle? (list 'a 'b 'c))) ; #f
(print (cycle? (make-cycle (list 'a 'b 'c)))) ; #t

; END