;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.06.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-variables exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (let-local-variables exp)
  (map car (let-variables exp)))

(define (let-local-expressions exp)
  (map cadr (let-variables exp)))

(define (let*->combination exp)
  (if (let-variables exp)
      '()
      (cons (make-lambda (let-local-variables exp)
                         (let-body exp))
            (let-local-expressions exp))))

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
    ((begin? exp)
     (eval-sequence (lambda-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
    (else
      (error "Unknown expression type -- EVAL" exp))))
