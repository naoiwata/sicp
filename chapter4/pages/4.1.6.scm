;;
;; @author naoiwata
;; SICP Chapter4
;; 4.1.6 Internal Definitions
;;

(define (f x)
  (define (even? n)
    (if (= n 0)
        #t
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        #f
        (even? (- n 1))))
  <rest of body of f>)

