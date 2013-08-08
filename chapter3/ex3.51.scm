;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.51.
;;

(add-load-path "./pages/" :relative)
(load "stream.scm")

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(define (show x)
  (display-line x)
  x)

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define x (stream-map show (stream-enumerate-interval 0 10)))
; 0x

(stream-ref x 5)
; 1
; 2
; 3
; 4
; 55

(stream-ref x 7)
; 6
; 77

; END