;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.37.
;;

; ------------------------------------------------------------------------
; import
; ------------------------------------------------------------------------

(add-load-path "." :relative)
(load "pages/3.3.5.scm")

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (celsius-fahrenheit-converter x)
  (c+ 
    (c* 
      (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let 
    ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let
    ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let
    ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let
    ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv val)
  (let
    ((z (make-connector)))
    (constant val z)
    z))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
(probe "celsius temp" C)
(probe "fahrenheit temp" F)

(set-value! C 25 'user)
; Probe: celsius temp = 25
; Probe: fahrenheit temp = 77

; (set-value! F 212 'user)
; "error": contradiction (77 212)

(forget-value! C 'user)
; Probe: celsius temp = ?
; Probe: fahrenheit temp = ?

(set-value! F 212 'user)
; Probe: fahrenheit temp = 212
; Probe: celsius temp = 100

; END