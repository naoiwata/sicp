;;
;; @author naoiwata
;; SICP Chapter3
;; 3.5.4 Streams and Delayed Evaluation
;;

(define int
  (cons-stream
    initial-value
    (add-streams (scale-stream integrand dt)
                 int)))

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
      initial-value
      (add-streams (scale-stream integrand dt)
                   int)))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)