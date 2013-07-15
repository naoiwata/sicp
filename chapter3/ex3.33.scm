;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.33.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (averager a b c)
  (let
    ((x (make-connector))
     (y (make-connector)))
    (adder a b x)
    (multiplier y c x)
    (constant 2 y)))

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "." :relative)
(load "pages/3.3.5.scm")

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)

(probe "A" A)
(probe "B" B)
(probe "Average" C)

(set-value! A 10 'user)
(set-value! B 20 'user)
; Probe: A = 10
; Probe: B = 20
; Probe: Average = 15

(forget-value! A 'user)
; Probe: A = ?
; Probe: Average = ?

(set-value! A 50 'user)
; Probe: A = 50
; Probe: Average = 35

; END