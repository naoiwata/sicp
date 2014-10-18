;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.03.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./" :relative)
(load "pages/4.1.1.scm")
(load "pages/4.1.2.scm")

(define eval-table (make-hash-table))

(define (put op type item)
  (hash-table-put! eval-table type item))

(define (get op type)
  (hash-table-get eval-table type))

(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((get (car exp))
     ((get (car exp)) exp env))
    ((application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
    (else
      (error "Unknown expression type -- EVAL" exp))))

(put eval-table
     'quote 
     (lambda (exp env)
       (text-of-quotation exp)))

(put eval-table
     'set!
     (lambda (exp env)
       (eval-assignment exp env)))

(put eval-table
     'define eval-definition)

(put eval-table 'if eval-if)

(put eval-table
     'lambda 
     (lambda (exp env)
       (make-procedure (lambda-parameters exp)
                       (lambda-body exp)
                       env)))

(put eval-table
     'begin
     (lambda (exp env)
       (eval-sequence (begin-actions exp) env)))

(put eval-table
     'cond
     (lambda (exp env)
        (eval (cond->if exp) env)))
