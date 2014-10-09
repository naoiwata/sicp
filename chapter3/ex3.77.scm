;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.77.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt))))

(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (let
                 ((integrand (force integrand)))
                   (if (stream-null? integrand)
                       the-empty-stream
                       (integral (delay (stream-cdr integrand))
                                 (+ (* dt (stream-car integrand))
                                    initial-value)
                                 dt)))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(print (stream-ref (solve (lambda (y) y) 1 0.001) 1000))
;; => 2.716923932235896
