;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.32.
;;

; ------------------------------------------------------------------------
; queue
; ------------------------------------------------------------------------

(define (make-queue)
  (let 
    ((front-ptr '())
     (rear-ptr '()))
    ;; internal procedures
    ; set! front
    (define (set-front-ptr! item) (set! front-ptr item))
    ; set! rear
    (define (set-rear-ptr! item)  (set! rear-ptr item))
    ; null? front
    (define (empty-queue?) (null? front-ptr))
    ; return front
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          front-ptr))
    ; insert queue to the rear
    (define (insert-queue! item)
      (let 
        ((new-pair (cons item '())))
        (cond 
          ((empty-queue?)
           (set-front-ptr! new-pair)
           (set-rear-ptr!  new-pair)
           (front-queue))
          (else
            (set-cdr! rear-ptr new-pair)
            (set-rear-ptr! new-pair)
            (front-queue)))))
    ; delete front
    (define (delete-queue!)
      (cond 
        ((empty-queue?)
         (error "DELETE! called with an empty queue"))
        (else
          (set-front-ptr! (cdr front-ptr))
          (front-queue))))
    ; dispatch
    (define (dispatch m)
      (cond
        ((eq? m 'insert-queue!) insert-queue!)
        ((eq? m 'delete-queue!) (delete-queue!))
        (else
          (error "Undefined operation -- MAKE-QUEUE" m))))
    dispatch))

(define (insert-queue! queue item) 
  ((queue 'insert-queue!) item))
(define (delete-queue! queue) 
  (queue 'delete-queue!))

; ------------------------------------------------------------------------
; import 3.3.4
; ------------------------------------------------------------------------

; Implementing the agenda
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) 
  (car s))

(define (segment-queue s) 
  (cdr s))

(define (make-agenda) 
  (list 0))

(define (current-time agenda) 
  (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) 
  (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) 
  (car (segments agenda)))

(define (rest-segments agenda) 
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let 
      ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let 
          ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
                segments
                (cons (make-new-time-segment time action)
                      (cdr segments)))
              (add-to-segments! rest)))))
  (let 
    ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
          agenda
          (cons (make-new-time-segment time action)
                segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let 
    ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty")
      (let 
        ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let 
        ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (after-delay delay action)
  (add-to-agenda! 
    (+ delay (current-time the-agenda))
    action
    the-agenda))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

; Representing wires
(define (make-wire)
  (let 
    ((signal-value 0) (action-procedures '()))
  (define (set-my-signal! new-value)
    (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
          (call-each action-procedures))
        'done))
  (define (accept-action-procedure! proc)
    (set! action-procedures (cons proc action-procedures))
    (proc))
  (define (dispatch m)
    (cond ((eq? m 'get-signal) signal-value)
          ((eq? m 'set-signal!) set-my-signal!)
          ((eq? m 'add-action!) accept-action-procedure!)
          (else 
            (error "Unknown operation" m))))
  dispatch))

; half-addr
(define (half-adder a b s c)
  (let 
    ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; several gates
(define (inverter input output)
  (define (invert-input)
    (let 
      ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else 
          (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let 
      ((new-value
         (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s c)
  (cond ((and (= s 1) (= c 1)) 1)
        ((or
           (and (= s 0) (= c 0))
           (and (= s 1) (= c 0))
           (and (= s 0) (= c 1))) 0)
        (else 
          (error "Invalid signal" s))))

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
; test
; ------------------------------------------------------------------------

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum) ; sum 0 New-value = 0 
(probe 'carry carry) ; carry 0 New-value = 0
(half-adder input-1 input-2 sum carry) ; ok
(set-signal! input-1 1) ; done
(propagate) ; ERROR

; END