;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.13.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------


(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp)
     (make-procedure (lambda-rparameters exp)
                     (lambda-body exp)
                     env))
    ((begin? exp)
     (eval-sequence (lambda-actions exp) env))
    ((unbound? exp)
     (eval-unbounding exp env))
    ((cond? exp) (eval (cond->if exp) env))
    ((application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
    (else
     (error "Unknown expression type -- EVAL" exp))))

(define (eval-unbounding exp env)
  (unbound-variable! (unbounding-variable exp) env)
  'ok)

(define (unbounding-variable exp)
  (cadr exp))

(define (unbound-variable! var exp)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (error "Unbound variable" var))
            ((eq? var (car vars))
             (set-car! vars (cadr vars))
             (set-cdr! vars (cddr vars))
             (set-car! vals (cadr vals))
             (set-cdr! vals (cddr vals)))
            (else
             (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
