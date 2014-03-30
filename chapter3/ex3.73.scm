;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.73.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC-circuit R C dt)
  (lambda (v0 i)
    (add-stream
      (integral 
        (scale-stream i (/ 1 C)) v0 dt)
      (scale-stream i R))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------
