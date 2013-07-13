;;
;; @author naoiwata
;; SICP Chapter3
;; 3.3.5 Propagation of Constraints
;;

; ------------------------------------------------------------------------
; Using the constraint system
; ------------------------------------------------------------------------

(define C (make-conector))
(define F (make-conector))
(celsius-fahrenheit-converter C F)

(define (celsius-fahrenheit-converter c f)
  (let
    ((u (make-conector))
     (v (make-conector))
     (w (make-conector))
     (x (make-conector))
     (y (make-conector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)))

; ------------------------------------------------------------------------
; Implementing the constraint system
; ------------------------------------------------------------------------

; adder
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
       (set-value! sum
                   (+ (get-value a1) (get-value a2))
                   me))
      ((and (has-value? a1) (has-value? sum))
       (set-value! a2
                   (- (get-value sum) (get-value a1))
                   me))
      ((and (has-value? a2) (has-value? sum))
       (set-value! a1
                   (- (get-value sum) (get-value a2))
                   me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1  me)
    (forget-value! a2  me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'i-have-a-value)
       (process-new-value))
      ((eq? request 'i-lost-my-value)
       (process-forget-value))
      (else
        (error "unknown request" request))))
  (connect a1  me)
  (connect a2  me)
  (connect sum me)
  me)

; multiplier
(define (multiplier m1 m2)
  (define (process-new-value)
    (cond
      ((or 
         (and (has-value? m1) (= (get-value? m1) 0))
         (and (has-value? m2) (= (get-value? m2) 0)))
       (set-value! product 0 me))
      ((and (has-value? m1) (has-value? m2))
       (set-value! product
                   (* (get-value m1) (get-value m2))
                   me))
      ((and (has-value? m1) (has-value? product))
       (set-value! m2
                   (/ (get-value product) (get-value m1))
                   me))
      ((and (has-value? m2) (has-value? product))
       (set-value! m1
                   (/ (get-value product) (get-value m2))
                   me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1      me)
    (forget-value! m2      me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'i-have-a-value)
       (process-new-value))
      ((eq? request 'i-lost-my-value)
       (process-forget-value))
      (else
        (error "unknown request" request))))
  (connect a1  me)
  (connect a2  me)
  (connect sum me)
  me)

; sub methods
(define (inform-about-value constraint)
  (constraint 'i-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'inform-about-no-value))

(define (constant value connector)
  (define (me request)
    (error "unknown request" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond
      ((eq? request 'i-have-a-value)
       (process-new-value))
      ((eq? request 'i-lost-my-value)
       (process-forget-value))
      (else
        (error "unknown request" request))))
  (connect connector me)
  me)

; ------------------------------------------------------------------------
; Representing connectors
; ------------------------------------------------------------------------

(define (make-conector)
  (let
    ((value #f)
     (informant #f)
     (constraints '()))
    (define (set-my-value newval setter)
      (cond
        ((not (has-value? me))
         (set! value newval)
         (set! informant setter)
         (for-each-except setter
                          inform-about-value
                          constraints))
        ((not (= value newval))
         (error "contradiction" (list value newval)))
        (else
          'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin 
            (set! informant #f)
            (for-each-except retractor
                             inform-about-no-value
                             constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond
        ((eq? request 'has-value?)
         (if informant #t #f))
        ((eq? request 'value)      value)
        ((eq? request 'set-value!) set-my-value)
        ((eq? request 'forget)     forget-my-value)
        ((eq? request 'connect)    connect)
        (else
          (error "unknown operation" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond
      ((null? items) 'dome)
      ((eq? (car items) exception) (loop (cdr items)))
      (else
        (procedure (car items))
        (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'get-value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget-value!) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; END