;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.02.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

; a

; Louis code
(define (eval exp env)
  (cond
    ((self-evalutating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp)
     (make-procedure (lambda-rparameters exp)
                     (lambda-body exp)
                     env))
    ((begin? exp)
     (eval-sequence (lambda-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    (else
      (error "Unknown expression type -- EVAL" exp))))

; Louis の定義した eval 手続きは、assignment? 手続きよりも application? 手続きが先に評価される。
; (define x 3) を評価しようとすると、define 手続きに引数 x, 3 を適応するような処理が実行され、本来意図していた x に 3 を代入しない。

; b

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (cddr exp))
