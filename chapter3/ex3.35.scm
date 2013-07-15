;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.35.
;;

; ------------------------------------------------------------------------
; question / solution
; ------------------------------------------------------------------------

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (set-value! b (square (get-value a)) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond 
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
        (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(add-load-path "." :relative)
(load "pages/3.3.5.scm")

(define square
  (lambda (x) (* x x)))

(define A (make-connector))
(define B (make-connector))
(squarer A B)

(probe "A" A)
(probe "Square" B)

(set-value! A 10 'user)
; Probe: A = 10
; Probe: Square = 100

(forget-value! A 'user)
; Probe: A = ?
; Probe: Square = ?

(set-value! A 20 'user)
; Probe: A = 20
; Probe: Square = 400

(forget-value! A 'user)
; Probe: A = ?
; Probe: Square = ?

(set-value! B 900 'user)
; Probe: Square = 900
; Probe: A = 30

; END