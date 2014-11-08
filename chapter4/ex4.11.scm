;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.11.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))
