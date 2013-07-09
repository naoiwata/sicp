;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.30.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (ripple-carry-adder ak bk sk ck)
  (let
    ((c (make-wire)))
    (if (null? (cdr ak))
        (set-signal! c 0)
        (ripple-carry-adder (cdr ak) (cdr bk) (cdr sk) c))
    (full-adder (car ak) (car bk) (car sk) ck)))

;  = full-adder-delay * n 
;  = (half-adder-delay * 2 + or-gate-delay) * n 
;  = ((or-gate-delay + and-gate-delay + inverter-delay + and-gate-delay) + or-gate-delay) * n
;  = (and-gate-delay * 4 + or-gate-delay * 3 + inverter-delay) * n

; has not test yet:(

; END