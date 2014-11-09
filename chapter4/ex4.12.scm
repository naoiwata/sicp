;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.12.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (env-loop env null-procedure eq-procedure error-procedure)
  (define (scan vars vals)
    (cond ((null? vars)
           (null-procedure env))
          ((eq? var (car vars))
           (car vals))
          (else
           (eq-procedure vals))))
  (if (eq? env the-empty-environment)
      (error-procedure var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (lookup-variable-value var env)
  (define (null-procedure e)
    (env-loop (enclosing-environment e) null-procedure eq-procedure))
  (define (eq-procedure vals)
    (car vals))
  (define (error-procedure var)
    (error "Unbound variable" var))
  (env-loop env null-procedure eq-procedure error-procedure))

(define (set-variables-value! var val env)
  (define (null-procedure e)
    (env-loop (enclosing-environment e) null-procedure eq-procedure))
  (define (eq-procedure vals)
    (set-cdr! vals val))
  (define (error-procedure var)
    (error "Unbound variable -- SET!" var))
  (env-loop env null-procedure eq-procedure error-procedure))

(define (define-variable! var val env)
  (define (null-procedure e)
    (add-binding-to-frame! var val (first-frame env)))
  (define (eq-procedure vals)
    (set-car! vals val))
  (define (error-procedure var)
    '())
  (env-loop env null-procedure eq-procedure error-procedure))
