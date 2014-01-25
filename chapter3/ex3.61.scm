;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.61.
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

(print
   (stream-ref
     (invert-unit-series integers) 0))
; 1

(print
   (stream-ref
     (invert-unit-series integers) 1))
; -2

; TODO:

; END