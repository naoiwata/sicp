;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.29.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

; or-gate
(define (or-gate a1 a2 output)
  (let
    ((c (make-wire))
     (d (make-wire))
     (e (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d)
    (inverter d output)
    'ok))

; ------------------------------------------------------------------------
; discussion
; ------------------------------------------------------------------------

; 遅延時間は (+ (* inverter-delay 3) and-gate-delay)

; END