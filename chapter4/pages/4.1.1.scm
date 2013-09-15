;;
;; @author naoiwata
;; SICP Chapter4
;; 4.1.1 The Core of the Evaluator
;;

; eval
(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookip-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-difinition exp env))
    ((if? exp) (eval-if exp env))
    ((lamnda? exp) (make-procedure
                     (lambda-parameters exp)
                     (lambda-body exp)
                     env))
    ((begin? exp) (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((application? exp) (apply
                          (eval (operator exp) env)
                          (list-of-values (operands exp) env)))
    (else
      (error "unknown expression" exp))))

; apply
(define (apply procedure arguments)
  (cond
    ((primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
     (eval-sequence
       (procedure-body procedure)
       (extend-environment
         (procedure-parameters procedure)
         arguments
         (procedure-environment procedure))))
    (else
      (error "unknown expression" procedure))))