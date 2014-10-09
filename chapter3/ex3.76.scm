;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.76.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (average a b) (/ (+ a b) 2))

(define (smooth s)
  (stream-map (lambda (a b) (average a b))
              (stream-cdr s) s))

(define (zero-crossings s)
  (stream-map sign-change-detector s (cons-stream 0 s)))
