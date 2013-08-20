;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.59.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

; (a)
(define (integrate-series S) ; S = a0, a1, a2, a3....
    (stream-map / S integers))

; (b)
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

; (cos x)' = - sin x
(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

; (sin x)' = cos x 
(define sine-series
  (cons-stream 0 integrate-series cosine-series))

; END