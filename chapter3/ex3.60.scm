;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.60.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-stream
                 (stream-map
                   (lambda (k) (* (stream-car s1) x))
                   (stream-car s2))
                 (mul-series (stream-cdr s1) s2))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integrate-series S)
    (stream-map / S integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(print 
  (stream-ref
    (add-streams
      (mul-series sine-series sine-series)
      (mul-series cosine-series cosine-series))
    0))
; 1

; END