;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.19.
;;

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (cycle? x)
  (define (cycle-iter a b)
    (cond
      ((eq? a b) #t)
      ((not (and (pair? b) (pair? (cdr b)))) #f)
      (else
        (cycle-iter (cdr a) (cddr b)))))
  (if (pair? x)
      (cycle-iter x (cdr x))
      #f))

; test
(define z0 '(a b c))
(print (cycle? z0)) ; #f
(define z1 (make-cycle '(a b c)))
(print (cycle? z1)) ; #t

; END