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
    ((get eval-table (car exp))
     ((get eval-table (car exp)) exp env))
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
     'define
     (lambda (exp env)
       (if (symbol? (cadr exp))
           (caadr exp)
           (make-lambda (cdadr exp)
                        (cddr exp)))))

(put eval-table
     'if
     (lambda (exp env)
       (if (true? (eval (if-predicate exp) env))
           (eval (if-consequent exp) env)
           (eval (if-alternative exp) env))))

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

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(print (eval 123 '())) ;; => 123

(print (eval "hoge" '())) ;; => hoge

(print (eval '(quote 10) '())) ;; => 10

(print (eval '(set! a 10) '()))

(print (eval '(define b 10) '()))
