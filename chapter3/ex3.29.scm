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
  (define (or-action-procedure)
    (let ((not-a1 (make-wire))
          (not-a2 (make-wire))
          (b (make-wire)))
      (inverter a1 not-a1)
      (inverter a2 not-a2)
      (and-gate not-a1 not-a2 b)
      (inverter b output)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; ------------------------------------------------------------------------
; discussion
; ------------------------------------------------------------------------

; 遅延時間は (+ (* inverter-delay 2) and-gate-delay)

; END