;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.07.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./" :relative)
(load "pages/4.1.1.scm")
(load "pages/4.1.2.scm")
(load "ex4.06.scm")

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*-variables exp)
  (cadr exp))

(define (let*-body exp)
  (cddr exp))

(define (let*->nested-lets exp)
  (define (let-iter arguments body)
    (if (null? (cdr arguments))
        (list 'let arguments body)
        (list 'let (list (car arguments)) (let-iter (cdr arguments) body))))
  (let-iter (let*-variables exp) (let*-body exp)))

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
    ((let*? exp)
     (eval (let*->nested-lets exp) env))
    ((begin? exp)
     (eval-sequence (lambda-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
    (else
      (error "Unknown expression type -- EVAL" exp))))
