;;
;; @author naoiwata
;; SICP Chapter3
;; 3.3.4 A Simulator for Digital Circuits
;;

; ------------------------------------------------------------------------
; a half-adder circuit
; ------------------------------------------------------------------------

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(define (half-adder a b s c)
  (let
    ((d (make-wire))
     (e (make-wire)))
    (or-gate  a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a v c-in sum c-out)
  (let 
    ((s (make-wire))
     (c1 (make-wire))
     (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; ------------------------------------------------------------------------
; Primitive function boxes
; ------------------------------------------------------------------------

; inverter
(define (inverter input output)
  (define (invert-input)
    (let
      ((new-value (logical-not (get-signal input))))
      (after-delay 
        inverter-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond
    ((= s 0) 1)
    ((= s 1) 0)
    (else
      (error "invalid signal" s))))

; and-gate
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let
      ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; ------------------------------------------------------------------------
; Representing wires
; ------------------------------------------------------------------------

(define (make-wire)
  (let
    ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done)))
  (define (accept-action-procedure! proc)
    (set! action-procedures (cons proc action-procedures))
    (proc))
  (define (dispatch m)
    (cond
      ((eq? m 'get-signal)  signal-value)
      ((eq? m 'set-signal)  set-my-signal!)
      ((eq? m 'add-action!) accept-action-procedure!)
      (else
        (error "error:(" m)))))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; ------------------------------------------------------------------------
; The agenda
; ------------------------------------------------------------------------

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let
        ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

; END