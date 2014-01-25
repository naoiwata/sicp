;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.62.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (invert-unit-series S)
  (cons-stream 1
               (stream-map -
                           (mul-series (stream-cdr S)
                                       (invert-unit-series S)))))

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "cannot divide with zero :(")
      (mul-series s1
                 (scale-stream (invert-unit-series s2)
                               (/ 1 (stream-car s2))))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-stream
                 (stream-map
                   (lambda (k) (* (stream-car s1) x))
                   (stream-car s2))
                 (mul-series (stream-cdr s1) s2))))
(define (integrate-series S) 
    (stream-map / S integers))
(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define tangent-series
  (div-series sine-series cosine-series))

; TODO
(print
  (stream-ref tangent-series 0))
(print
  (stream-ref tangent-series 1))
(print
  (stream-ref tangent-series 2))

; END