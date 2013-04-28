;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.2.
;;

(define (make-monitored f)
  (define mf 0)
  (define (how-many-calls?) mf)
  (define (reset-count) (set! mf 0) mf)
  (define (dispatch m)
    (cond
      ((eq? m 'how-many-calls?) (how-many-calls?))
      ((eq? m 'reset-count) (reset-count))
      (else
        (set! mf (+ mf 1))
        (f m))))
  dispatch)

; test
(define s (make-monitored sqrt))
(print (s 100)) ; 10
(print (s 'how-many-calls?)) ; 1
(print (s 400)) ; 20
(print (s 'how-many-calls?)) ; 2
(print (s 'reset-count)) ; 0
(print (s 'how-many-calls?)) ; 0
(print (s 100)) ; 10
(print (s 'how-many-calls?)) ; 1

; END