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
               (scale-stream
                 (mul-series (stream-cdr S)
                             (invert-unit-series S))
                 -1)))

; END