;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.6.
;;

(define rand
  (let
    ((x 1))
    (define (dispatch m)
      (cond
        ((eq? m 'reset)
         (lambda (i)
           (begin
             (set! x i)
             x)))
        ((eq? m 'generate)
         (begin 
           (set! x (rand-update x))
           x))
        (else
          (error "Unknown request -- RAND" m))))
    dispatch))

(define (rand-update x)
  (let
    ((a 13)
     (b 17)
     (m 371))
    (modulo (+ (* a x) b) m)))

; test
(print (rand 'generate)) ; 30
(print (rand 'generate)) ; 36
(print (rand 'generate)) ; 114
(print (rand 'generate)) ; 15
(print ((rand 'reset) 2)) ; 2
(print (rand 'generate)) ; 43
(print (rand 'generate)) ; 205
(print (rand 'generate)) ; 85
(print (rand 'generate)) ; 9
(print ((rand 'reset) 1)) ; 2
(print (rand 'generate)) ; 30
(print (rand 'generate)) ; 36
(print (rand 'generate)) ; 114
(print (rand 'generate)) ; 15

; END