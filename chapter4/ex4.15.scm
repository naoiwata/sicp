;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.15.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (run-forever) (run-forever))

(define (try p)
  (id (halts? p p)
      (run-forever)
      'halted))
