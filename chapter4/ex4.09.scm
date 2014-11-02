;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.09.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (while? exp)
  (tagged-list? exp 'white))

(define (while-condition exp)
  (cadr exp))

(define (while-body exp)
  (caddr exp))

(define (while->named-let exp)
  (list 'let
        'while
        '()
        (make-if (while-named-let exp)
                 (make-begin (list (car while-body exp)
                                   '(while)))
                 '())))

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
    ((let? exp)
     (eval (let*->combination exp) env))
    ((while? exp)
     (eval (while->named-let exp) env))
    ((begin? exp)
     (eval-sequence (lambda-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
    (else
      (error "Unknown expression type -- EVAL" exp))))
